/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.io.IOException;
import java.util.UUID;
import java.util.function.Function;

import com.cloudway.fp.io.IOFunction;
import com.cloudway.platform.container.adapters.LinuxContainerAdapter;
import com.cloudway.platform.container.adapters.MacOSContainerAdapter;
import com.cloudway.platform.container.adapters.UnixContainerAdapter;

import jnr.ffi.Platform;
import static jnr.ffi.Platform.OS.*;
import static com.cloudway.fp.control.Conditionals.*;
import static com.cloudway.fp.control.Predicates.*;

final class Helper {
    private Helper() {}

    private static final IOFunction<ApplicationContainer, ApplicationRepository>
        DEFAULT_REPOSITORY_FACTORY = GitRepository::new;

    private static IOFunction<ApplicationContainer, ApplicationRepository>
        repositoryFactory = DEFAULT_REPOSITORY_FACTORY;

    private static Function<ApplicationContainer, ContainerAdapter>
        adapterFactory = getDefaultContainerAdapterFactory();

    static void setApplicationRepositoryFactory(IOFunction<ApplicationContainer, ApplicationRepository> factory) {
        repositoryFactory = factory != null ? factory : DEFAULT_REPOSITORY_FACTORY;
    }

    static ApplicationRepository getApplicationRepository(ApplicationContainer container)
        throws IOException {
        return repositoryFactory.evaluate(container);
    }

    static void setContainerAdapterFactory(Function<ApplicationContainer, ContainerAdapter> factory) {
        adapterFactory = factory != null ? factory : getDefaultContainerAdapterFactory();
    }

    static ContainerAdapter createContainerAdapter(ApplicationContainer container) {
        return adapterFactory.apply(container);
    }

    // @formatter:off
    private static Function<ApplicationContainer, ContainerAdapter> getDefaultContainerAdapterFactory() {
        Platform platform = Platform.getNativePlatform();
        return with().<Function<ApplicationContainer, ContainerAdapter>>get()
          .when(platform.getOS(), is(LINUX),  () -> LinuxContainerAdapter::new)
          .when(platform.getOS(), is(DARWIN), () -> MacOSContainerAdapter::new)
          .when(platform::isUnix,             () -> UnixContainerAdapter::new)
          .orElse(c -> { throw new IllegalStateException("unsupported platform: " + platform.getName()); });
    }
    // @formatter:on

    static String makeUUID() {
        UUID uuid = UUID.randomUUID();
        return digits(uuid.getMostSignificantBits()) + digits(uuid.getLeastSignificantBits());
    }

    private static String digits(long val) {
        String str = Long.toHexString(val);
        while (str.length() < 16)
            str = "0" + str;
        return str;
    }
}
