/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package cloudway;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import com.cloudway.platform.common.util.FileUtils;
import com.cloudway.platform.common.util.IO;
import com.cloudway.platform.container.ApplicationContainer;
import com.cloudway.platform.container.ApplicationState;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.OptionBuilder;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.PosixParser;

public class PrivilegedControl extends Control
{
    @Command("Show application information")
    public void info(String[] args) throws IOException {
        if (args.length == 0) {
            System.out.printf(" %-32s %-16s %s%n", "UUID", "Name", "DNS");
            System.out.printf("=============================================================================%n");
            ApplicationContainer.all().forEach(c -> {
                System.out.printf("%c%-32s %-16s %s%n",
                                  c.getState() == ApplicationState.STARTED ? '*' : ' ',
                                  c.getUuid(), c.getName(), c.getDomainName());
            });
        } else {
            command("info", args, false, this::showInfo);
        }
    }

    private void showInfo(ApplicationContainer container) {
        System.out.println("UUID:  " + container.getUuid());
        System.out.println("Name:  " + container.getName());
        System.out.println("DNS:   " + container.getDomainName());
        System.out.println("Size:  " + container.getCapacity());
        System.out.println("State: " + container.getState());
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

    @Command("Destroy application container(s)")
    public void destroy(String[] args) throws IOException {
        command("destroy", args, true, ApplicationContainer::destroy);
    }

    private void command(String name, String[] args, boolean parallel, IO.Consumer<ApplicationContainer> action)
        throws IOException
    {
        if (args.length == 0) {
            System.err.println("usage: cwctl " + name + " [all | uuid...]");
            System.exit(1);
        }

        if (args.length == 1 && "all".equals(args[0])) {
            Stream<ApplicationContainer> stream = ApplicationContainer.all();
            IO.forEach(parallel ? stream.parallel() : stream, action);
        } else {
            IO.forEach(Stream.of(args), key -> do_action(key, action));
        }
    }

    private void do_action(String key, IO.Consumer<ApplicationContainer> action)
        throws IOException
    {
        ApplicationContainer container;

        if (key.indexOf('.') != -1) {
            // assume the key is a FQDN
            container = ApplicationContainer.all()
                .filter(c -> key.equals(c.getDomainName()))
                .findFirst()
                .orElse(null);
        } else if (key.indexOf('-') != -1) {
            // assume the key is "name-namespace"
            container = ApplicationContainer.all()
                .filter(c -> key.equals(c.getName() + "-" + c.getNamespace()))
                .findFirst()
                .orElse(null);
        } else {
            // assume the key is an UUID
            try {
                container = ApplicationContainer.fromUuid(key);
            } catch (Exception ex) {
                container = null;
            }
        }

        if (container == null) {
            System.err.println(key + ": not found");
        } else {
            action.accept(container);
        }
    }

    @SuppressWarnings("all")
    private static Option[] CREATE_OPTIONS = {
        OptionBuilder.withArgName("capacity")
                     .withDescription("Application capacity (small,medium,large)")
                     .hasArg()
                     .create('c'),
        OptionBuilder.withArgName("keyfile")
                     .withDescription("SSH public key file")
                     .hasArg()
                     .create("k"),
        OptionBuilder.withArgName("add-on")
                     .withDescription("Add-on source location")
                     .hasArg()
                     .create('s'),
        OptionBuilder.withArgName("repository")
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

        String uuid        = mkuuid();
        String name        = matcher.group(1);
        String namespace   = matcher.group(2);
        String capacity    = cmd.getOptionValue("c", "small");
        String keyfile     = loadKeyFile(cmd.getOptionValue("k"));
        String source      = cmd.getOptionValue("s");
        String repo        = cmd.getOptionValue("r");

        ApplicationContainer container =
            ApplicationContainer.create(uuid, name, namespace, capacity);

        if (keyfile != null) {
            container.addAuthorizedKey("default", keyfile);
        }

        if (source != null) {
            install(container, source, repo);
            container.start();
        }
    }

    private void printHelp(Options options) {
        List<Option> order = Arrays.asList(CREATE_OPTIONS);
        Comparator<Option> c = (o1,o2) -> order.indexOf(o1) - order.indexOf(o2);

        HelpFormatter formatter = new HelpFormatter();
        formatter.setOptionComparator(c);
        formatter.printHelp("cwctl create [OPTION]... <name>-<namespace>", options);
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
            System.err.println("usage: cwctl install uuid source [repo]");
            System.exit(1);
            return;
        }

        String uuid   = args[0];
        String source = args[1];
        String repo   = args.length > 2 ? args[2] : null;

        do_action(uuid, container -> install(container, source, repo));
    }

    @Command("Uninstall add-on from application")
    public void uninstall(String args[]) throws IOException {
        if (args.length != 2) {
            System.err.println("usage: cwctl uninstall uuid name");
            System.exit(1);
            return;
        }

        do_action(args[0], container -> container.remove(args[1]));
    }
}
