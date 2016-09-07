package cmds

import (
	"archive/tar"
	"compress/gzip"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"net/url"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"

	"github.com/docker/go-units"
	"golang.org/x/net/context"

	"github.com/cloudway/platform/api/types"
	"github.com/cloudway/platform/cmd/cwcli/cmds/ansi"
	"github.com/cloudway/platform/config"
	"github.com/cloudway/platform/pkg/archive"
	"github.com/cloudway/platform/pkg/manifest"
	"github.com/cloudway/platform/pkg/mflag"
	"github.com/cloudway/platform/pkg/opts"
)

const appCmdUsage = `Usage: cwcli app

list applications

Additional commands, type "cwcli help COMMAND" for more details:

  app:create         Create a new application
  app:remove         Permanently remove an application
  app:start          Start an application
  app:stop           Stop an application
  app:restart        Restart an application
  app:status         Show application status
  app:ps             Show application processes
  app:stats          Display application live resource usage statistics
  app:service        Manage application services
  app:clone          Clone application source code
  app:deploy         Deploy an application
  app:upload         Upload an application repository
  app:dump           Dump application data
  app:restore        Restore application data
  app:scale          Scale an application
  app:info           Show application information
  app:env            Get or set application environment variables
  app:open           Open the application in a web brower
  app:ssh            Log into application console via SSH
`

func (cli *CWCli) CmdApps(args ...string) error {
	var help bool

	cmd := cli.Subcmd("app", "")
	cmd.Require(mflag.Exact, 0)
	cmd.BoolVar(&help, []string{"-help"}, false, "Print usage")
	cmd.ParseFlags(args, false)

	if help {
		fmt.Fprintln(cli.stdout, appCmdUsage)
		os.Exit(0)
	}

	if err := cli.ConnectAndLogin(); err != nil {
		return err
	}

	if apps, err := cli.GetApplications(context.Background()); err != nil {
		return err
	} else {
		for _, name := range apps {
			fmt.Fprintln(cli.stdout, name)
		}
	}

	return nil
}

func (cli *CWCli) getAppName(cmd *mflag.FlagSet) string {
	if cmd != nil {
		if appf := cmd.Lookup("a"); appf != nil {
			if name := appf.Value.String(); name != "" {
				return name
			}
		}
	}

	if name := cli.getAppConfig("app"); name != "" {
		return name
	}
	if name := gitGetConfig("cloudway.app"); name != "" {
		return name
	}

	fmt.Fprintln(cli.stdout, "Missing application name in command line arguments.")
	os.Exit(1)
	return ""
}

func (cli *CWCli) getAppConfig(key string) string {
	root, err := searchFile(".cwapp")
	if err != nil {
		return ""
	}
	cfg, err := config.Open(filepath.Join(root, ".cwapp"))
	if err != nil {
		return ""
	}
	return cfg.Get(key)
}

func (cli *CWCli) getAppRoot() (root string, binary bool, err error) {
	root, err = searchFile(".git")
	if err == nil {
		return
	}

	root, err = searchFile(".cwapp")
	binary = true
	return
}

func searchFile(name string) (string, error) {
	pwd, err := os.Getwd()
	if err != nil {
		return "", err
	}

	for {
		file := filepath.Join(pwd, name)
		_, err := os.Lstat(file)
		if err == nil {
			return pwd, nil
		}
		if !os.IsNotExist(err) {
			return "", err
		}
		if parent := filepath.Dir(pwd); parent != pwd {
			pwd = parent
		} else {
			return "", errors.New("The current directory is not a valid cloudway application")
		}
	}
}

func (cli *CWCli) writeJson(obj interface{}) {
	if ansi.IsTerminal {
		b, _ := json.MarshalIndent(obj, "", "  ")
		cli.stdout.Write(b)
		fmt.Fprintln(cli.stdout)
	} else {
		json.NewEncoder(os.Stdout).Encode(obj)
	}
}

func (cli *CWCli) CmdAppInfo(args ...string) error {
	var js bool

	cmd := cli.Subcmd("app:info", "")
	cmd.Require(mflag.Exact, 0)
	cmd.String([]string{"a", "-app"}, "", "Specify the application name")
	cmd.BoolVar(&js, []string{"-json"}, false, "Display as JSON")
	cmd.ParseFlags(args, true)
	name := cli.getAppName(cmd)

	if err := cli.ConnectAndLogin(); err != nil {
		return err
	}

	app, err := cli.GetApplicationInfo(context.Background(), name)
	if err != nil {
		return err
	}

	if js {
		cli.writeJson(&app)
	} else {
		fmt.Fprintf(cli.stdout, "Name:       %s\n", app.Name)
		fmt.Fprintf(cli.stdout, "Namespace:  %s\n", app.Namespace)
		fmt.Fprintf(cli.stdout, "Created:    %v\n", app.CreatedAt)
		fmt.Fprintf(cli.stdout, "Framework:  %s\n", app.Framework.DisplayName)
		fmt.Fprintf(cli.stdout, "Scaling:    %v\n", app.Scaling)
		fmt.Fprintf(cli.stdout, "URL:        %s\n", app.URL)
		fmt.Fprintf(cli.stdout, "Clone URL:  %s\n", app.CloneURL)
		fmt.Fprintf(cli.stdout, "SSH URL:    %s\n", app.SSHURL)
		fmt.Fprintf(cli.stdout, "Services:\n")
		for _, p := range app.Services {
			fmt.Fprintf(cli.stdout, " - %s\n", p.DisplayName)
		}
	}

	return nil
}

func (cli *CWCli) CmdAppOpen(args ...string) error {
	cmd := cli.Subcmd("app:open", "")
	cmd.Require(mflag.Exact, 0)
	cmd.String([]string{"a", "-app"}, "", "Specify the application name")
	cmd.ParseFlags(args, true)
	name := cli.getAppName(cmd)

	if err := cli.ConnectAndLogin(); err != nil {
		return err
	}

	app, err := cli.GetApplicationInfo(context.Background(), name)
	if err != nil {
		return err
	}
	return openurl(app.URL)
}

func (cli *CWCli) CmdAppClone(args ...string) error {
	var binary bool

	cmd := cli.Subcmd("app:clone", "NAME")
	cmd.Require(mflag.Exact, 1)
	cmd.BoolVar(&binary, []string{"-binary"}, false, "Download binary repository")
	cmd.ParseFlags(args, true)
	name := cmd.Arg(0)

	if _, err := os.Stat(name); !os.IsNotExist(err) {
		if err == nil {
			err = fmt.Errorf("destination path '%s' already exists", name)
		}
		return err
	}

	if err := cli.ConnectAndLogin(); err != nil {
		return err
	}

	if binary {
		return cli.download(name)
	} else {
		app, err := cli.GetApplicationInfo(context.Background(), name)
		if err == nil {
			err = gitClone(cli.host, app, true)
		}
		return err
	}
}

func (cli *CWCli) CmdAppUpload(args ...string) error {
	cmd := cli.Subcmd("app:upload", "")
	cmd.Require(mflag.Exact, 0)
	cmd.String([]string{"a", "-app"}, "", "Specify the application name")
	cmd.ParseFlags(args, true)

	name := cli.getAppName(cmd)
	path, binary, err := cli.getAppRoot()
	if err != nil {
		return err
	}

	if err := cli.ConnectAndLogin(); err != nil {
		return err
	}

	return cli.upload(name, path, binary)
}

func (cli *CWCli) download(name string) error {
	r, err := cli.Download(context.Background(), name)
	if err != nil {
		return err
	}
	defer r.Close()

	dir, err := filepath.Abs(name)
	if err != nil {
		return err
	}
	if err = os.Mkdir(dir, 0755); err != nil {
		return err
	}

	zr, err := gzip.NewReader(r)
	if err != nil {
		return err
	}
	if err = archive.ExtractFiles(dir, zr); err != nil {
		return err
	}

	cfg := config.New(filepath.Join(dir, ".cwapp"))
	cfg.Set("host", cli.host)
	cfg.Set("app", name)
	return cfg.Save()
}

func (cli *CWCli) upload(name, path string, binary bool) error {
	// create temporary archive file containing upload files
	tempfile, err := ioutil.TempFile("", "deploy")
	if err != nil {
		return err
	}
	defer func() {
		tempfile.Close()
		os.Remove(tempfile.Name())
	}()

	zw := gzip.NewWriter(tempfile)
	tw := tar.NewWriter(zw)
	excludes := []string{".git", ".cwapp"}
	if err = archive.CopyFileTree(tw, "", path, excludes, false); err != nil {
		return err
	}
	tw.Close()
	zw.Close()

	// rewind for read
	if _, err = tempfile.Seek(0, os.SEEK_SET); err != nil {
		return err
	}

	return cli.Upload(context.Background(), name, tempfile, binary, cli.stdout)
}

func (cli *CWCli) CmdAppDump(args ...string) (err error) {
	var output string

	cmd := cli.Subcmd("app:dump", "")
	cmd.Require(mflag.Exact, 0)
	cmd.String([]string{"a", "-app"}, "", "Specify the application name")
	cmd.StringVar(&output, []string{"o"}, "", "Specify the output file")
	cmd.ParseFlags(args, true)
	name := cli.getAppName(cmd)

	if err := cli.ConnectAndLogin(); err != nil {
		return err
	}

	var out *os.File
	if output == "" {
		out = os.Stdout
	} else {
		out, err = os.Create(output)
		if err != nil {
			return err
		}
		defer out.Close()
	}

	r, err := cli.Dump(context.Background(), name)
	if err != nil {
		return err
	}
	defer r.Close()

	_, err = io.Copy(out, r)
	return err
}

func (cli *CWCli) CmdAppRestore(args ...string) (err error) {
	var input string

	cmd := cli.Subcmd("app:restore", "")
	cmd.Require(mflag.Exact, 0)
	cmd.String([]string{"a", "-app"}, "", "Specify the application name")
	cmd.StringVar(&input, []string{"i"}, "", "Specify the input file")
	cmd.ParseFlags(args, true)
	name := cli.getAppName(cmd)

	if err := cli.ConnectAndLogin(); err != nil {
		return err
	}

	var in *os.File
	if input == "" {
		in = os.Stdin
	} else {
		in, err = os.Open(input)
		if err != nil {
			return err
		}
		defer in.Close()
	}

	return cli.Restore(context.Background(), name, in)
}

func (cli *CWCli) CmdAppSSH(args ...string) error {
	var name, service, identity string

	cmd := cli.Subcmd("app:ssh", "")
	cmd.Require(mflag.Exact, 0)
	cmd.String([]string{"a", "-app"}, "", "Specify the application name")
	cmd.StringVar(&service, []string{"s", "-service"}, "", "Service name")
	cmd.StringVar(&identity, []string{"i"}, "", "Identity file")
	cmd.ParseFlags(args, true)
	name = cli.getAppName(cmd)

	if err := cli.ConnectAndLogin(); err != nil {
		return err
	}

	app, err := cli.GetApplicationInfo(context.Background(), name)
	if err != nil {
		return err
	}
	if app.SSHURL == "" {
		return errors.New("Cannot determine the SSH URL")
	}

	sshurl, err := url.Parse(app.SSHURL)
	if err != nil {
		return err
	}

	var sshCmdArgs []string

	host, port := sshurl.Host, ""
	if i := strings.IndexRune(host, ':'); i != -1 {
		host, port = host[:i], host[i+1:]
		sshCmdArgs = append(sshCmdArgs, "-p", port)
	}
	if identity != "" {
		sshCmdArgs = append(sshCmdArgs, "-i", identity)
	}

	container := sshurl.User.Username()
	if service != "" {
		container = service + "." + container
	}
	sshCmdArgs = append(sshCmdArgs, container+"@"+host)

	sshCmd := exec.Command("ssh", sshCmdArgs...)
	sshCmd.Stdin = os.Stdin
	sshCmd.Stdout = os.Stdout
	sshCmd.Stderr = os.Stderr
	return sshCmd.Run()
}

func (cli *CWCli) CmdAppCreate(args ...string) error {
	var req types.CreateApplication
	var noclone, binary bool

	cmd := cli.Subcmd("app:create", "[OPTIONS] NAME")
	cmd.Require(mflag.Exact, 1)
	cmd.StringVar(&req.Framework, []string{"F", "-framework"}, "", "Application framework")
	cmd.Var(opts.NewListOptsRef(&req.Services, nil), []string{"s", "-service"}, "Service plugins")
	cmd.StringVar(&req.Repo, []string{"-repo"}, "", "Populate from a repository")
	cmd.BoolVar(&noclone, []string{"n", "-no-clone"}, false, "Do not clone source code")
	cmd.BoolVar(&binary, []string{"-binary"}, false, "Download binary repository")
	cmd.ParseFlags(args, true)
	req.Name = cmd.Arg(0)

	if !noclone {
		if _, err := os.Stat(req.Name); !os.IsNotExist(err) {
			if err == nil {
				err = fmt.Errorf("destination path '%s' already exists", req.Name)
			}
			return err
		}
	}

	if err := cli.ConnectAndLogin(); err != nil {
		return err
	}

	app, err := cli.CreateApplication(context.Background(), req, cli.stdout)
	if err != nil {
		return err
	}
	if !noclone {
		if binary {
			return cli.download(req.Name)
		} else if app.CloneURL != "" {
			return gitClone(cli.host, app, false)
		}
	}
	return nil
}

func (cli *CWCli) CmdAppRemove(args ...string) error {
	var yes bool

	cmd := cli.Subcmd("app:remove", "NAME")
	cmd.Require(mflag.Exact, 1)
	cmd.BoolVar(&yes, []string{"y"}, false, "Confirm 'yes' to remove the application")
	cmd.ParseFlags(args, true)

	if !yes && !cli.confirm("You will lost all your application data") {
		return nil
	}
	if err := cli.ConnectAndLogin(); err != nil {
		return err
	}
	return cli.RemoveApplication(context.Background(), cmd.Arg(0))
}

func (cli *CWCli) CmdAppStart(args ...string) error {
	cmd := cli.Subcmd("app:start", "")
	cmd.Require(mflag.Exact, 0)
	cmd.String([]string{"a", "-app"}, "", "Specify the application name")
	cmd.ParseFlags(args, true)
	name := cli.getAppName(cmd)

	if err := cli.ConnectAndLogin(); err != nil {
		return err
	}
	return cli.StartApplication(context.Background(), name)
}

func (cli *CWCli) CmdAppStop(args ...string) error {
	cmd := cli.Subcmd("app:stop", "")
	cmd.Require(mflag.Exact, 0)
	cmd.String([]string{"a", "-app"}, "", "Specify the application name")
	cmd.ParseFlags(args, true)
	name := cli.getAppName(cmd)

	if err := cli.ConnectAndLogin(); err != nil {
		return err
	}
	return cli.StopApplication(context.Background(), name)
}

func (cli *CWCli) CmdAppRestart(args ...string) error {
	cmd := cli.Subcmd("app:restart", "")
	cmd.Require(mflag.Exact, 0)
	cmd.String([]string{"a", "-app"}, "", "Specify the application name")
	cmd.ParseFlags(args, true)
	name := cli.getAppName(cmd)

	if err := cli.ConnectAndLogin(); err != nil {
		return err
	}
	return cli.RestartApplication(context.Background(), name)
}

func (cli *CWCli) CmdAppStatus(args ...string) error {
	var all, js bool
	var name string

	cmd := cli.Subcmd("app:status", "")
	cmd.Require(mflag.Exact, 0)
	cmd.String([]string{"a", "-app"}, "", "Specify the application name")
	cmd.BoolVar(&all, []string{"-all"}, false, "Display all application status")
	cmd.BoolVar(&js, []string{"-json"}, false, "Display as JSON")
	cmd.ParseFlags(args, true)

	if !all {
		name = cli.getAppName(cmd)
	}
	if err := cli.ConnectAndLogin(); err != nil {
		return err
	}

	var header = []string{"ID", "NAME", "DISPLAY NAME", "IP ADDRESS", "PORTS", "UP TIME", "STATE"}
	var addRow = func(tab *Table, s *types.ContainerStatus) {
		ports := strings.Join(s.Ports, ",")
		uptime := units.HumanDuration(time.Duration(s.Uptime))
		tab.AddRow(s.ID[:12], s.Name, s.DisplayName, s.IPAddress, ports, uptime, wrapState(s.State))
	}

	if all {
		status, err := cli.GetAllApplicationStatus(context.Background())
		if err != nil {
			return err
		}
		if js {
			cli.writeJson(status)
		} else {
			tab := NewTable(header...)
			tab.SetColor(0, ansi.NewColor(ansi.FgYellow))
			tab.SetColor(1, ansi.NewColor(ansi.FgCyan))
			for name, st := range status {
				tab.AddSubtitle(ansi.Info(name))
				for _, s := range st {
					addRow(tab, s)
				}
			}
			tab.Display(cli.stdout, 3)
		}
	} else {
		st, err := cli.GetApplicationStatus(context.Background(), name)
		if err != nil {
			return err
		}
		if js {
			cli.writeJson(st)
		} else {
			tab := NewTable(header...)
			tab.SetColor(0, ansi.NewColor(ansi.FgYellow))
			tab.SetColor(1, ansi.NewColor(ansi.FgCyan))
			for _, s := range st {
				addRow(tab, s)
			}
			tab.Display(cli.stdout, 3)
		}
	}

	return nil
}

func wrapState(state manifest.ActiveState) string {
	switch state {
	case manifest.StateRunning:
		return ansi.Success(state.String())
	case manifest.StateStarting, manifest.StateRestarting, manifest.StateStopping:
		return ansi.Warning(state.String())
	case manifest.StateBuilding:
		return ansi.Info(state.String())
	default:
		return ansi.Fail(state.String())
	}
}

func (cli *CWCli) CmdAppPs(args ...string) error {
	var js bool

	cmd := cli.Subcmd("app:ps", "")
	cmd.Require(mflag.Exact, 0)
	cmd.String([]string{"a", "-app"}, "", "Specify the application name")
	cmd.BoolVar(&js, []string{"-json"}, false, "Display as JSON")
	cmd.ParseFlags(args, true)
	name := cli.getAppName(cmd)

	if err := cli.ConnectAndLogin(); err != nil {
		return err
	}

	procs, err := cli.GetApplicationProcesses(context.Background(), name)
	if err != nil {
		return err
	}

	if len(procs) == 0 {
		return nil
	}

	if js {
		cli.writeJson(procs)
		return nil
	}

	for _, pl := range procs {
		io.WriteString(cli.stdout, ansi.Warning(pl.ID[:12])+" "+ansi.Info(pl.DisplayName+"\n"))
		tab := NewTable(pl.Headers...)
		for _, row := range pl.Processes {
			tab.AddRow(row...)
		}
		tab.Display(cli.stdout, 1)
		fmt.Fprintln(cli.stdout)
	}
	return nil
}

func (cli *CWCli) CmdAppStats(args ...string) error {
	cmd := cli.Subcmd("app:stats", "")
	cmd.Require(mflag.Exact, 0)
	cmd.String([]string{"a", "-app"}, "", "Specify the application name")
	cmd.ParseFlags(args, true)
	name := cli.getAppName(cmd)

	if err := cli.ConnectAndLogin(); err != nil {
		return err
	}

	resp, err := cli.GetApplicationStats(context.Background(), name)
	if err != nil {
		return err
	}
	defer resp.Close()

	dec := json.NewDecoder(resp)
	for {
		var stats []*types.ContainerStats
		err = dec.Decode(&stats)
		if err == io.EOF {
			return nil
		}
		if err != nil {
			return err
		}

		tab := NewTable("ID", "NAME", "%CPU", "%MEM", "MEM USAGE / LIMIT", "NET I/O R/W", "BLOCK I/O R/W")
		tab.SetColor(0, ansi.NewColor(ansi.FgYellow))
		tab.SetColor(1, ansi.NewColor(ansi.FgCyan))
		for _, s := range stats {
			tab.AddRow(
				s.ID[:12],
				s.Name,
				fmt.Sprintf("%.2f%%", s.CPUPercentage),
				fmt.Sprintf("%.2f%%", s.MemoryPercentage),
				units.BytesSize(float64(s.MemoryUsage))+" / "+units.BytesSize(float64(s.MemoryLimit)),
				units.HumanSize(float64(s.NetworkRx))+" / "+units.HumanSize(float64(s.NetworkTx)),
				units.HumanSize(float64(s.BlockRead))+" / "+units.HumanSize(float64(s.BlockWrite)))
		}
		io.WriteString(cli.stdout, "\033[2J\033[H")
		tab.Display(cli.stdout, 2)
	}
}

func (cli *CWCli) CmdAppDeploy(args ...string) error {
	var branch string
	var show bool

	cmd := cli.Subcmd("app:deploy", "")
	cmd.Require(mflag.Exact, 0)
	cmd.String([]string{"a", "-app"}, "", "Specify the application name")
	cmd.StringVar(&branch, []string{"b", "-branch"}, "", "The branch to deploy")
	cmd.BoolVar(&show, []string{"-show"}, false, "Show application deployments")
	cmd.ParseFlags(args, true)
	name := cli.getAppName(cmd)

	if err := cli.ConnectAndLogin(); err != nil {
		return err
	}

	if show {
		deployments, err := cli.GetApplicationDeployments(context.Background(), name)
		if err != nil {
			return err
		}

		var display = func(ref *types.Branch) {
			display := ref.DisplayId
			if ref.Id == deployments.Current.Id {
				display = "* " + ansi.Hilite(display)
			} else {
				display = "  " + display
			}
			fmt.Fprintf(cli.stdout, "%s\n", display)
		}

		fmt.Fprintln(cli.stdout, "Branches:")
		for _, ref := range deployments.Branches {
			if ref.Type == "BRANCH" {
				display(ref)
			}
		}
		fmt.Fprintln(cli.stdout)

		fmt.Fprintln(cli.stdout, "Tags:")
		for _, ref := range deployments.Branches {
			if ref.Type == "TAG" {
				display(ref)
			}
		}

		return nil
	} else {
		return cli.DeployApplication(context.Background(), name, branch, cli.stdout)
	}
}

func (cli *CWCli) CmdAppScale(args ...string) error {
	cmd := cli.Subcmd("app:scale", "NAME [+|-]SCALING")
	cmd.Require(mflag.Exact, 2)
	cmd.ParseFlags(args, true)

	name, scale := cmd.Arg(0), cmd.Arg(1)
	if name == "." {
		name = cli.getAppName(nil)
	}
	if err := cli.ConnectAndLogin(); err != nil {
		return err
	}
	return cli.ScaleApplication(context.Background(), name, scale)
}

func (cli *CWCli) CmdAppEnv(args ...string) error {
	var service string
	var del bool
	var showPassword bool

	cmd := cli.Subcmd("app:env", "", "KEY", "KEY=VALUE...", "-d KEY...")
	cmd.String([]string{"a", "-app"}, "", "Application name")
	cmd.StringVar(&service, []string{"s", "-service"}, "", "Service name")
	cmd.BoolVar(&del, []string{"d"}, false, "Remove the environment variable")
	cmd.BoolVar(&showPassword, []string{"p", "-show-password"}, false, "Show password environment variable values")
	cmd.ParseFlags(args, true)
	name := cli.getAppName(cmd)

	if err := cli.ConnectAndLogin(); err != nil {
		return err
	}

	ctx := context.Background()

	if del {
		// cwcli app:env -d key1 key2 ...
		return cli.ApplicationUnsetenv(ctx, name, service, cmd.Args()...)
	}

	switch {
	case cmd.NArg() == 0:
		// cwcli app:env
		env, err := cli.ApplicationEnviron(ctx, name, service)
		if err != nil {
			return err
		}

		if !showPassword {
			var passwords []string
			for k, v := range env {
				if strings.Contains(strings.ToLower(k), "password") {
					passwords = append(passwords, v)
					env[k] = strings.Repeat("*", len(v))
				}
			}
			if len(passwords) != 0 {
				for k, v := range env {
					for _, pass := range passwords {
						if strings.Contains(v, pass) {
							env[k] = strings.Replace(v, pass, strings.Repeat("*", len(pass)), -1)
						}
					}
				}
			}
		}

		for k, v := range env {
			fmt.Fprintf(cli.stdout, "%s=%s\n", k, v)
		}

	case cmd.NArg() == 1 && !strings.ContainsRune(cmd.Arg(0), '='):
		// cwcli app:env key
		val, err := cli.ApplicationGetenv(ctx, name, service, cmd.Arg(0))
		if err != nil {
			return err
		}
		fmt.Fprintln(cli.stdout, val)

	default:
		// cwcli app:env key1=val1 key2=val2 ...
		env := make(map[string]string)
		for i := 0; i < cmd.NArg(); i++ {
			kv := cmd.Arg(i)
			if sep := strings.IndexRune(kv, '='); sep > 1 {
				env[kv[:sep]] = kv[sep+1:]
			} else {
				cmd.Usage()
				os.Exit(1)
			}
		}
		return cli.ApplicationSetenv(ctx, name, service, env)
	}

	return nil
}

const appServiceUsage = `Usage: cwcli app:service [COMMAND]

Manage application services.

Additional commands, type "cwcli help app:service COMMAND" for more details:

  add                Add services to the application
  remove             Remove a service from the applicaiton
`

func (cli *CWCli) CmdAppService(args ...string) error {
	var help bool

	cmd := cli.Subcmd("app:service", "")
	cmd.Require(mflag.Exact, 0)
	cmd.BoolVar(&help, []string{"-help"}, false, "Print usage")
	cmd.String([]string{"a", "-app"}, "", "Specify the application name")
	cmd.ParseFlags(args, false)

	if help {
		fmt.Fprintln(cli.stdout, appServiceUsage)
		os.Exit(0)
	}

	name := cli.getAppName(cmd)

	if err := cli.ConnectAndLogin(); err != nil {
		return err
	}

	app, err := cli.GetApplicationInfo(context.Background(), name)
	if err != nil {
		return err
	}
	for _, p := range app.Services {
		fmt.Fprintf(cli.stdout, "%-12.12s%s\n", p.Name, p.DisplayName)
	}
	return nil
}

func (cli *CWCli) CmdAppServiceAdd(args ...string) error {
	cmd := cli.Subcmd("app:service add", "SERVICES...")
	cmd.Require(mflag.Min, 1)
	cmd.String([]string{"a", "-app"}, "", "Specify the application name")
	cmd.ParseFlags(args, true)

	name := cli.getAppName(cmd)
	tags := cmd.Args()

	if err := cli.ConnectAndLogin(); err != nil {
		return err
	}

	return cli.CreateService(context.Background(), cli.stdout, name, tags...)
}

func (cli *CWCli) CmdAppServiceRemove(args ...string) error {
	var yes bool

	cmd := cli.Subcmd("app:service remove", "SERVICE")
	cmd.Require(mflag.Exact, 1)
	cmd.String([]string{"a", "-app"}, "", "Specify the application name")
	cmd.BoolVar(&yes, []string{"y"}, false, "Confirm 'yes' to remove the service")
	cmd.ParseFlags(args, true)

	name := cli.getAppName(cmd)
	service := cmd.Arg(0)

	if !yes && !cli.confirm("You will lost all your service data") {
		return nil
	}

	if err := cli.ConnectAndLogin(); err != nil {
		return err
	}

	return cli.RemoveService(context.Background(), name, service)
}
