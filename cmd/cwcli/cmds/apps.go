package cmds

import (
    "fmt"
    "io"
    "os"
    "strings"
    "errors"
    "bufio"
    "os/exec"
    "net/url"
    "encoding/json"
    "golang.org/x/net/context"
    "github.com/cloudway/platform/pkg/mflag"
    "github.com/cloudway/platform/pkg/opts"
    "github.com/cloudway/platform/api/types"
)

const appCmdUsage = `Usage: cwcli app

list applications

Additional commands, type "cwcli help COMMAND" for more details:

  app:create         Create a new application
  app:remove         Permanently remove an application
  app:start          Start an application
  app:stop           Stop an application
  app:restart        Restart an application
  app:deploy         Deploy an application
  app:scale          Scale an application
  app:info           Show application information
  app:env            Get or set application environment variables
  app:open           Open the application in a web brower
  app:clone          Clone application source code
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

func (cli *CWCli) CmdAppInfo(args ...string) error {
    var js bool

    cmd := cli.Subcmd("app:info", "NAME")
    cmd.Require(mflag.Exact, 1)
    cmd.BoolVar(&js, []string{"-json"}, false, "Display as JSON")
    cmd.ParseFlags(args, true)

    if err := cli.ConnectAndLogin(); err != nil {
        return err
    }

    app, err := cli.GetApplicationInfo(context.Background(), cmd.Arg(0))
    if err != nil {
        return err
    }

    if js {
        b, _ := json.MarshalIndent(&app, "", "   ")
        fmt.Fprintln(cli.stdout, string(b))
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
    cmd := cli.Subcmd("app:open", "NAME")
    cmd.Require(mflag.Exact, 1)
    cmd.ParseFlags(args, true)

    if err := cli.ConnectAndLogin(); err != nil {
        return err
    }

    app, err := cli.GetApplicationInfo(context.Background(), cmd.Arg(0))
    if err != nil {
        return err
    }
    return openurl(app.URL)
}

func (cli *CWCli) CmdAppClone(args ...string) error {
    cmd := cli.Subcmd("app:clone", "NAME")
    cmd.Require(mflag.Exact, 1)
    cmd.ParseFlags(args, true)

    if err := cli.ConnectAndLogin(); err != nil {
        return err
    }

    app, err := cli.GetApplicationInfo(context.Background(), cmd.Arg(0))
    if err != nil {
        return err
    }
    if app.CloneURL == "" {
        return errors.New("Cannot determine the clone command")
    }

    cloneCmdArgs := strings.Fields(app.CloneURL)
    cloneCmd := exec.Command(cloneCmdArgs[0], cloneCmdArgs[1:]...)
    cloneCmd.Stdout = os.Stdout
    cloneCmd.Stderr = os.Stderr
    return cloneCmd.Run()
}

func (cli *CWCli) CmdAppSSH(args ...string) error {
    var identity string

    cmd := cli.Subcmd("app:ssh", "[SERVICE.]NAME")
    cmd.Require(mflag.Exact, 1)
    cmd.StringVar(&identity, []string{"i"}, "", "Identity file")
    cmd.ParseFlags(args, true)

    if err := cli.ConnectAndLogin(); err != nil {
        return err
    }

    name, service := cmd.Arg(0), ""
    if i := strings.IndexRune(name, '.'); i != -1 {
        name, service = name[i+1:], name[:i]
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

    cmd := cli.Subcmd("app:create", "[OPTIONS] NAME")
    cmd.Require(mflag.Exact, 1)
    cmd.StringVar(&req.Framework, []string{"F", "-framework"}, "", "Application framework")
    cmd.Var(opts.NewListOptsRef(&req.Services, nil), []string{"s", "-service"}, "Service plugins")
    cmd.StringVar(&req.Repo, []string{"-repo"}, "", "Populate from a repository")
    cmd.ParseFlags(args, true)
    req.Name = cmd.Arg(0)

    if err := cli.ConnectAndLogin(); err != nil {
        return err
    }
    return cli.CreateApplication(context.Background(), req)
}


func (cli *CWCli) CmdAppRemove(args ...string) error {
    var yes bool

    cmd := cli.Subcmd("app:remove", "NAME")
    cmd.Require(mflag.Exact, 1)
    cmd.BoolVar(&yes, []string{"y"}, false, "Confirm 'yes' to remove the application")
    cmd.ParseFlags(args, true)

    if !yes {
        reader := bufio.NewReader(os.Stdin)
        for {
            fmt.Fprintf(cli.stdout, alert("WARNING")+": You will lost all your application data, continue (yes/no)? ")
            answer, err := reader.ReadString('\n')
            if err == io.EOF {
                return nil
            }
            if err != nil {
                return err
            }
            answer = strings.TrimSpace(answer)
            if answer == "no" || answer == "" {
                return nil
            }
            if answer == "yes" {
                break
            }
            fmt.Fprintln(cli.stdout, "Please answer yes or no.")
        }
    }

    if err := cli.ConnectAndLogin(); err != nil {
        return err
    }
    return cli.RemoveApplication(context.Background(), cmd.Arg(0))
}

func (cli *CWCli) CmdAppStart(args ...string) error {
    cmd := cli.Subcmd("app:start", "NAME")
    cmd.Require(mflag.Exact, 1)
    cmd.ParseFlags(args, true)

    if err := cli.ConnectAndLogin(); err != nil {
        return err
    }
    return cli.StartApplication(context.Background(), cmd.Arg(0))
}

func (cli *CWCli) CmdAppStop(args ...string) error {
    cmd := cli.Subcmd("app:stop", "NAME")
    cmd.Require(mflag.Exact, 1)
    cmd.ParseFlags(args, true)

    if err := cli.ConnectAndLogin(); err != nil {
        return err
    }
    return cli.StopApplication(context.Background(), cmd.Arg(0))
}

func (cli *CWCli) CmdAppRestart(args ...string) error {
    cmd := cli.Subcmd("app:restart", "NAME")
    cmd.Require(mflag.Exact, 1)
    cmd.ParseFlags(args, true)

    if err := cli.ConnectAndLogin(); err != nil {
        return err
    }
    return cli.RestartApplication(context.Background(), cmd.Arg(0))
}

func (cli *CWCli) CmdAppDeploy(args ...string) error {
    var show bool

    cmd := cli.Subcmd("app:deploy", "NAME [BRANCH]")
    cmd.Require(mflag.Min, 1)
    cmd.Require(mflag.Max, 2)
    cmd.BoolVar(&show, []string{"-show"}, false, "Show application deployments")
    cmd.ParseFlags(args, true)

    name, branch := cmd.Arg(0), ""
    if cmd.NArg() == 2 {
        branch = cmd.Arg(1)
    }

    if err := cli.ConnectAndLogin(); err != nil {
        return err
    }

    if show {
        deployments, err := cli.GetApplicationDeployments(context.Background(), name)
        if err != nil {
            return err
        }

        var display = func(ref types.Branch) {
            display := ref.DisplayId
            if ref.Id == deployments.Current.Id {
                display = hilite("*"+display)
            } else {
                display = " "+display
            }
            fmt.Fprintf(cli.stdout, " %s\n", display)
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
        return cli.DeployApplication(context.Background(), name, branch)
    }
}

func (cli *CWCli) CmdAppScale(args ...string) error {
    cmd := cli.Subcmd("app:scale", "NAME [+|-]SCALING")
    cmd.Require(mflag.Exact, 2)
    cmd.ParseFlags(args, true)

    if err := cli.ConnectAndLogin(); err != nil {
        return err
    }
    return cli.ScaleApplication(context.Background(), cmd.Arg(0), cmd.Arg(1))
}

func (cli *CWCli) CmdAppEnv(args ...string) error {
    var name, service string
    var del bool

    cmd := cli.Subcmd("app:env", "NAME", "NAME KEY", "NAME KEY=VALUE...", "-d KEY...")
    cmd.Require(mflag.Min, 1)
    cmd.StringVar(&service, []string{"s", "-service"}, "", "Service name")
    cmd.BoolVar(&del, []string{"d"}, false, "Remove the environment variable")
    cmd.ParseFlags(args, true)
    name = cmd.Arg(0)

    if err := cli.ConnectAndLogin(); err != nil {
        return err
    }

    if del {
        // cwcli app:env myapp key1 key2 ...
        return cli.ApplicationUnsetenv(context.Background(), name, service, cmd.Args()[1:]...)
    }

    switch {
    case cmd.NArg() == 1:
        // cwcli app:env myapp
        env, err := cli.ApplicationEnviron(context.Background(), name, service)
        if err != nil {
            return err
        }
        for k, v := range env {
            fmt.Fprintf(cli.stdout, "%s=%s\n", k, v)
        }

    case cmd.NArg() == 2 && !strings.ContainsRune(cmd.Arg(1), '='):
        // cwcli app:env myapp key
        val, err := cli.ApplicationGetenv(context.Background(), name, service, cmd.Arg(1))
        if err != nil {
            return err
        }
        fmt.Fprintln(cli.stdout, val)

    default:
        // cwcli app:env myapp key1=val1 key2=val2 ...
        env := make(map[string]string)
        for i := 1; i < cmd.NArg(); i++ {
            kv := cmd.Arg(i)
            if sep := strings.IndexRune(kv, '='); sep > 1 {
                env[kv[:sep]] = kv[sep+1:]
            } else {
                cmd.Usage()
                os.Exit(1)
            }
        }
        return cli.ApplicationSetenv(context.Background(), name, service, env)
    }

    return nil
}
