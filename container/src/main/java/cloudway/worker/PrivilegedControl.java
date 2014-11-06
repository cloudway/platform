/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package cloudway.worker;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.cloudway.platform.common.util.Exec;
import com.cloudway.platform.common.util.FileUtils;
import com.cloudway.platform.common.util.IO;
import com.cloudway.platform.container.ApplicationContainer;
import com.cloudway.platform.container.NoSuchContainerException;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.OptionBuilder;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.PosixParser;

@SuppressWarnings("unused")
public class PrivilegedControl extends Control
{
    @Command("Show application information")
    public void info(String[] args) throws IOException {
        if (args.length == 0) {
            ApplicationContainer.all().forEach(c -> {
                System.out.printf("%-32s %-30s %s%n", c.getId(), c.getDomainName(), c.getState());
            });
        } else {
            command("info", args, false, this::showInfo);
        }
    }

    private void showInfo(ApplicationContainer container) {
        System.out.println("ID:      " + container.getId());
        System.out.println("Name:    " + container.getName());
        System.out.println("DNS:     " + container.getDomainName());
        System.out.println("Size:    " + container.getCapacity());
        System.out.println("State:   " + container.getState());
        System.out.println();
    }

    @Command("Start application container(s)")
    public void start(String[] args) throws IOException {
        command("start", args, true, ApplicationContainer::start);
    }

    @Command("Stop application container(s)")
    public void stop(String[] args) throws IOException {
        command("stop", args, true, ApplicationContainer::stop);
    }

    @Command("Restart application container(s)")
    public void restart(String[] args) throws IOException {
        command("restart", args, true, ApplicationContainer::restart);
    }

    @Command("Idle application container(s)")
    public void idle(String[] args) throws IOException {
        command("idle", args, true, ApplicationContainer::idle);
    }

    @Command("Unidle application container(s)")
    public void unidle(String[] args) throws IOException {
        command("unidle", args, true, ApplicationContainer::unidle);
    }

    @Command("Show current container status")
    public void status(String[] args) throws IOException {
        command("status", args, false, c -> {
            System.out.printf("%s (%s):%n", c.getId(), c.getDomainName());
            c.control("status", false);
            System.out.println();
        });
    }

    @Command("Destroy application container(s)")
    public void destroy(String[] args) throws IOException {
        command("destroy", args, true, ApplicationContainer::destroy);
    }

    private void command(String name, String[] args, boolean parallel, IO.Consumer<ApplicationContainer> action)
        throws IOException
    {
        if (args.length == 0) {
            System.err.println("usage: cwctl " + name + " [all | ID...]");
            System.exit(1);
        }

        if (args.length == 1 && "all".equals(args[0])) {
            Stream<ApplicationContainer> stream = ApplicationContainer.all();
            IO.forEach(parallel ? stream.parallel() : stream, action);
        } else {
            IO.forEach(Stream.of(args), id -> do_action(id, action));
        }
    }

    private void do_action(String id, IO.Consumer<ApplicationContainer> action)
        throws IOException
    {
        List<ApplicationContainer> containers;

        if (id.indexOf('.') != -1) {
            // assume the key is a FQDN
            containers = ApplicationContainer.all()
                .filter(c -> id.equals(c.getDomainName()))
                .collect(Collectors.toList());
        } else if (id.indexOf('-') != -1) {
            // assume the key is "name-namespace"
            containers = ApplicationContainer.all()
                .filter(c -> id.equals(c.getName() + "-" + c.getNamespace()))
                .collect(Collectors.toList());
        } else {
            // assume the key is an application id
            try {
                containers = Collections.singletonList(ApplicationContainer.fromId(id));
            } catch (NoSuchContainerException ex) {
                System.err.println(id + ": " + ex.getMessage());
                System.exit(2);
                return;
            }
        }

        if (containers.isEmpty()) {
            System.err.println(id + ": Not found");
            System.exit(2);
        } else {
            IO.forEach(containers, action);
        }
    }

    @SuppressWarnings("all")
    private static Option[] CREATE_OPTIONS = {
        OptionBuilder.withArgName("SIZE")
                     .withDescription("Application capacity (small,medium,large)")
                     .hasArg()
                     .create('c'),
        OptionBuilder.withArgName("SCALE")
                     .withDescription("Application scaling")
                     .hasArg()
                     .create('s'),
        OptionBuilder.withArgName("FILE")
                     .withDescription("SSH public key file")
                     .hasArg()
                     .create("k"),
        OptionBuilder.withArgName("PATH")
                     .withDescription("Add-on source location")
                     .hasArgs()
                     .create('a'),
        OptionBuilder.withArgName("URL")
                     .withDescription("Repository URL")
                     .hasArg()
                     .create('r')
    };

    @Command("Create a new application container")
    public void create(String[] args) throws IOException {
        Options options = new Options();
        Stream.of(CREATE_OPTIONS).forEach(options::addOption);

        CommandLine cmd;
        try {
            CommandLineParser parser = new PosixParser();
            cmd = parser.parse(options, args);
        } catch (ParseException ex) {
            printHelp(options);
            System.exit(1);
            return;
        }

        args = cmd.getArgs();
        if (args.length != 1) {
            printHelp(options);
            System.exit(1);
            return;
        }

        Pattern re = Pattern.compile("^([a-z][a-z_0-9]*)-([a-z][a-z_0-9]*)$");
        Matcher matcher = re.matcher(args[0]);
        if (!matcher.matches()) {
            System.err.println("the name and namespace arguments can only contains " +
                               "lower case letters, digits, or underscore");
            System.exit(1);
            return;
        }

        String name        = matcher.group(1);
        String namespace   = matcher.group(2);
        String capacity    = cmd.getOptionValue("c", "small");
        int    scale       = getScaling(name, namespace, cmd.getOptionValue("s"));
        String keyfile     = loadKeyFile(cmd.getOptionValue("k"));
        String sources[]   = cmd.getOptionValues("a");
        String repo        = cmd.getOptionValue("r");

        List<ApplicationContainer> containers = new ArrayList<>(scale);

        try {
            for (int i = 0; i < scale; i++) {
                ApplicationContainer container =
                    ApplicationContainer.create(mkuuid(), name, namespace, capacity);
                containers.add(container);

                if (keyfile != null) {
                    container.addAuthorizedKey("default", keyfile);
                }

                if (sources != null) {
                    IO.forEach(Stream.of(sources), source -> install(container, source, repo));
                    container.start();
                }
            }
        } catch (IOException | RuntimeException ex) {
            IO.forEach(containers, ApplicationContainer::destroy);
            throw ex;
        }
    }

    private void printHelp(Options options) {
        HelpFormatter formatter = new HelpFormatter();
        formatter.printHelp("cwctl create [OPTION]... NAME-NAMESPACE", options);
    }

    private static String mkuuid() {
        UUID uuid = UUID.randomUUID();
        return digits(uuid.getLeastSignificantBits()) + digits(uuid.getLeastSignificantBits());
    }

    private static String digits(long val) {
        String str = Long.toHexString(val);
        while (str.length() < 16)
            str = "0" + str;
        return str;
    }

   private static int getScaling(String name, String namespace, String scale) {
        int cc = 1;
        if (scale != null) {
            try {
                cc = Integer.parseInt(scale);
                if (cc <= 0) {
                    throw new NumberFormatException();
                }
            } catch (NumberFormatException ex) {
                System.err.println("Invalid scaling value, it must be an integer value greater than 0");
                System.exit(1);
            }
        }

        String fqdn = name + "-" + namespace + "." + ApplicationContainer.DOMAIN;
        int n = (int)ApplicationContainer.all().filter(c -> fqdn.equals(c.getDomainName())).count();
        if (cc <= n) {
            System.err.println("Application containers already reached maximum scaling value. " +
                               "(maximum scaling = " + cc + ", existing containers = " + n + ")");
            System.exit(1);
        }

        return cc - n;
    }

    private static String loadKeyFile(String filename) {
        Path file;

        if (filename != null) {
            file = Paths.get(filename);
        } else {
            file = Paths.get(System.getProperty("user.home"), ".ssh", "id_rsa.pub");
            if (!Files.exists(file)) {
                return null;
            }
        }

        try {
            return FileUtils.read(file);
        } catch (IOException ex) {
            System.err.println("failed to load public key file: " + ex.getMessage());
            System.exit(2);
            return null;
        }
    }

    @Command("Install add-on into application")
    public void install(String args[]) throws IOException {
        if (args.length < 2 || args.length > 3) {
            System.err.println("usage: cwctl install ID SOURCE [REPO]");
            System.exit(1);
            return;
        }

        String id     = args[0];
        String source = args[1];
        String repo   = args.length > 2 ? args[2] : null;

        do_action(id, container -> install(container, source, repo));
    }

    @Command("Uninstall add-on from application")
    public void uninstall(String args[]) throws IOException {
        if (args.length != 2) {
            System.err.println("usage: cwctl uninstall ID NAME");
            System.exit(1);
            return;
        }

        do_action(args[0], container -> container.remove(args[1]));
    }

    /** Convenient internal command to substitute container user. */
    public void su(String args[]) throws IOException {
        if (args.length == 0 || "all".equals(args[0])) {
            System.err.println("usage cwctl su <id or domain name>");
            System.exit(1);
            return;
        }

        if (args.length == 1) {
            do_action(args[0], container -> Exec.args("su", "-", container.getId()).run());
        } else {
            do_action(args[0], container -> container.join(
                Exec.args(Arrays.copyOfRange(args, 1, args.length))).run());
        }
    }

    /** Convenient internal command to run ssh on specified container user. */
    public void ssh(String args[]) throws IOException {
        if (args.length != 1 || "all".equals(args[0])) {
            System.err.println("usage cwctl su <id or domain name>");
            System.exit(1);
            return;
        }

        do_action(args[0], container -> Exec.args("ssh", container.getId() + "@localhost").run());
    }
}
