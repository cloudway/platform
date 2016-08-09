package cmds

import (
    "fmt"
    "os"
    "strings"
    "errors"
    "runtime"
    "bufio"
    "os/exec"
    "net/url"
    "encoding/json"
    "golang.org/x/net/context"
    "github.com/cloudway/platform/pkg/mflag"
    "github.com/cloudway/platform/pkg/opts"
    "github.com/cloudway/platform/pkg/manifest"
)

const usage = `Usage: cwcli apps

list applications

Additional commands, type "cwcli help COMMAND" for more details:

  app:create         Create a new application
  app:remove         Permanently remove an application
  app:info           Show application information
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
        fmt.Fprintln(os.Stdout, usage)
        os.Exit(0)
    }

    if err := cli.ConnectAndLogin(); err != nil {
        return err
    }

    if apps, err := cli.GetApplications(context.Background()); err != nil {
        return err
    } else {
        for _, name := range apps {
            fmt.Println(name)
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
        fmt.Println(string(b))
    } else {
        fmt.Printf("Name:       %s\n", app.Name)
        fmt.Printf("Namespace:  %s\n", app.Namespace)
        fmt.Printf("Created:    %v\n", app.CreatedAt)
        fmt.Printf("URL:        %s\n", app.URL)
        fmt.Printf("Clone URL:  %s\n", app.CloneURL)
        fmt.Printf("SSH URL:    %s\n", app.SSHURL)
        fmt.Printf("Framework:  %s\n", app.Framework.DisplayName)
        fmt.Println("Services:")
        for _, p := range app.Services {
            fmt.Printf(" - %s\n", p.DisplayName)
        }
    }

    return nil
}

func (cli *CWCli) CmdAppOpen(args ...string) error {
    if runtime.GOOS != "windows" && runtime.GOOS != "darwin" {
        return nil
    }

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

    var cmdArgs []string
    switch runtime.GOOS {
    case "windows":
        cmdArgs = []string{"cmd.exe", "/c",  "start "+app.URL}
    case "darwin":
        cmdArgs = []string{"open", app.URL}
    default:
        return nil
    }

    openCmd := exec.Command(cmdArgs[0], cmdArgs[1:]...)
    return openCmd.Run()
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
    var req manifest.CreateApplication

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
            fmt.Print(hilite("WARNING")+": You will lost all your application data, continue (yes/no)? ")
            answer, err := reader.ReadString('\n')
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
            fmt.Println("Please answer yes or no.")
        }
    }

    if err := cli.ConnectAndLogin(); err != nil {
        return err
    }

    return cli.RemoveApplication(context.Background(), cmd.Arg(0))
}

func hilite(text string) string {
    if runtime.GOOS == "windows" {
        return text
    } else {
        return "\033[31;1m" + text + "\033[0m"
    }
}
