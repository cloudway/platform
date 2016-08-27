package cmds

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/cloudway/platform/api/client"
	"github.com/cloudway/platform/config"
	"github.com/cloudway/platform/pkg/cli"
	flag "github.com/cloudway/platform/pkg/mflag"
)

// Command is the struct containing the command name and description
type Command struct {
	Name        string
	Description string
}

type CWCli struct {
	*cli.Cli
	host string
	*client.APIClient
	stdout, stderr io.Writer
	handlers       map[string]func(...string) error
}

// Commands lists the top level commands and their short usage
var CommandUsage = []Command{
	{"login", "Login to a Cloudway server"},
	{"logout", "Log out from a Cloudway server"},
	{"namespace", "Get or set application namespace"},
	{"app", "Manage applications"},
	{"app:create", "Create application"},
	{"app:remove", "Permanently remove an application"},
	{"app:start", "Start an application"},
	{"app:stop", "Stop an application"},
	{"app:restart", "Restart an application"},
	{"app:clone", "Clone application source code"},
	{"app:deploy", "Deploy an application"},
	{"app:upload", "Upload an application repository"},
	{"app:dump", "Dump application data"},
	{"app:restore", "Restore application data"},
	{"app:scale", "Scale an application"},
	{"app:info", "Show application information"},
	{"app:env", "Get or set application environment variables"},
	{"app:open", "Open the application in a web brower"},
	{"app:ssh", "Log into application console via SSH"},
	{"app:service:add", "Add new service to the application"},
	{"app:service:rm", "Remove service from the application"},
	{"plugin", "Show plugin information"},
	{"version", "Show the version information"},
}

var Commands = make(map[string]Command)

func init() {
	for _, cmd := range CommandUsage {
		Commands[cmd.Name] = cmd
	}
}

func Init(host string, stdout, stderr io.Writer) *CWCli {
	c := new(CWCli)
	c.Cli = cli.New("cwcli", c)
	c.Description = "Cloudway client interface"
	c.host = host
	c.stdout = stdout
	c.stderr = stderr

	c.handlers = map[string]func(...string) error{
		"login":           c.CmdLogin,
		"logout":          c.CmdLogout,
		"namespace":       c.CmdNamespace,
		"app":             c.CmdApps,
		"app:create":      c.CmdAppCreate,
		"app:remove":      c.CmdAppRemove,
		"app:start":       c.CmdAppStart,
		"app:stop":        c.CmdAppStop,
		"app:restart":     c.CmdAppRestart,
		"app:clone":       c.CmdAppClone,
		"app:deploy":      c.CmdAppDeploy,
		"app:upload":      c.CmdAppUpload,
		"app:dump":        c.CmdAppDump,
		"app:restore":     c.CmdAppRestore,
		"app:scale":       c.CmdAppScale,
		"app:info":        c.CmdAppInfo,
		"app:env":         c.CmdAppEnv,
		"app:open":        c.CmdAppOpen,
		"app:ssh":         c.CmdAppSSH,
		"app:service:add": c.CmdAppServiceAdd,
		"app:service:rm":  c.CmdAppServiceRemove,
		"plugin":          c.CmdPlugin,
		"version":         c.CmdVersion,
	}

	return c
}

func (c *CWCli) Command(name string) func(...string) error {
	return c.handlers[name]
}

func (c *CWCli) Subcmd(name string, synopses ...string) *flag.FlagSet {
	var description string
	if cmd, ok := Commands[name]; ok {
		description = cmd.Description
	}
	return c.Cli.Subcmd(name, synopses, description, true)
}

func (c *CWCli) Connect() (err error) {
	if c.APIClient != nil {
		return nil
	}

	if c.host == "" {
		c.host = c.getAppConfig("host")
		if c.host == "" {
			c.host = gitGetConfig("cloudway.host")
		}
		if c.host == "" {
			c.host = config.Get("host")
		}
		if c.host == "" {
			return errors.New("No remote host specified, please run cwcli with -H option")
		}
	}

	headers := map[string]string{
		"Accept": "application/json",
	}

	c.APIClient, err = client.NewAPIClient(c.host+"/api", "", nil, headers)
	return err
}

func (c *CWCli) ConnectAndLogin() (err error) {
	if err = c.Connect(); err != nil {
		return err
	}

	token := config.GetOption(c.host, "token")
	if token != "" {
		c.SetToken(token)
	} else {
		err = c.authenticate("You must login.", "", "")
	}
	return err
}

func (cli *CWCli) confirm(prompt string) bool {
	reader := bufio.NewReader(os.Stdin)
	for {
		fmt.Fprintf(cli.stdout, alert("WARNING")+": "+prompt+", continue (yes/no)? ")
		answer, err := reader.ReadString('\n')
		if err == io.EOF {
			return false
		}
		if err != nil {
			return false
		}
		answer = strings.TrimSpace(answer)
		if answer == "no" || answer == "" {
			return false
		}
		if answer == "yes" {
			return true
		}
		fmt.Fprintln(cli.stdout, "Please answer yes or no.")
	}
}
