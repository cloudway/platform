package cmds

import (
    "fmt"
    "os"
    "strings"
    "errors"
    "runtime"
    "os/exec"
    "net/url"
    "encoding/json"
    "golang.org/x/net/context"
    "github.com/cloudway/platform/pkg/mflag"
)

const usage = `Usage: cwcli apps

list applications

Additional commands, type "cwcli help COMMAND" for more details:

  app:create         Create a new application
  app:destroy        Permanently destroy an application
  app:info           Show application information
  app:log            Show application logs
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
    cmd := cli.Subcmd("app:ssh", "NAME")
    cmd.Require(mflag.Exact, 1)
    cmd.StringVar(&identity, []string{"i"}, "", "Identity file")
    cmd.ParseFlags(args, true)

    if err := cli.ConnectAndLogin(); err != nil {
        return err
    }

    app, err := cli.GetApplicationInfo(context.Background(), cmd.Arg(0))
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
    sshCmdArgs = append(sshCmdArgs, sshurl.User.Username()+"@"+host)

    sshCmd := exec.Command("ssh", sshCmdArgs...)
    sshCmd.Stdin = os.Stdin
    sshCmd.Stdout = os.Stdout
    sshCmd.Stderr = os.Stderr
    return sshCmd.Run()
}
