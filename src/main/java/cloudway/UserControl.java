/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package cloudway;

import java.io.IOException;
import com.cloudway.platform.container.ApplicationContainer;

@SuppressWarnings("unused")
public class UserControl extends Control
{
    private ApplicationContainer getContainer() {
        try {
            String uuid = System.getenv("CLOUDWAY_APP_UUID");
            return ApplicationContainer.fromUuid(uuid);
        } catch (Exception ex) {
            System.err.println("This program must run in a cloudway container.");
            System.exit(2);
            return null;
        }
    }

    @Command("Show application information")
    public void info(String[] args) {
        ApplicationContainer container = getContainer();
        System.out.println("UUID:  " + container.getUuid());
        System.out.println("Name:  " + container.getName());
        System.out.println("DNS:   " + container.getDomainName());
        System.out.println("Size:  " + container.getCapacity());
        System.out.println("State: " + container.getState());
    }

    @Command("Start the application container")
    public void start(String[] args)
        throws IOException
    {
        getContainer().start();
    }

    @Command("Stop the application container")
    public void stop(String[] args)
        throws IOException
    {
        getContainer().stop();
    }

    @Command("Cleanup application data")
    public void tidy(String[] args)
        throws IOException
    {
        getContainer().tidy();
    }

    public void pre_receive(String[] args)
        throws IOException
    {
        getContainer().pre_receive();
    }

    public void post_receive(String[] args)
        throws IOException
    {
        getContainer().post_receive();
    }
}
