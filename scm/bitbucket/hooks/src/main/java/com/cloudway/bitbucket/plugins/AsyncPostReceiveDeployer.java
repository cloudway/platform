/**
 * Cloudway Platform
 * Copyright (c) 2012-2016 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.bitbucket.plugins;

import java.util.Collection;

import com.atlassian.bitbucket.hook.HookService;
import com.atlassian.bitbucket.hook.repository.AsyncPostReceiveRepositoryHook;
import com.atlassian.bitbucket.hook.repository.RepositoryHookContext;
import com.atlassian.bitbucket.repository.RefChange;
import com.atlassian.bitbucket.scm.git.GitScmConfig;
import com.atlassian.bitbucket.scm.git.command.GitCommandBuilderFactory;

@SuppressWarnings("unused")
public class AsyncPostReceiveDeployer implements AsyncPostReceiveRepositoryHook {
    private final RepoDeployer deployer;

    AsyncPostReceiveDeployer(GitCommandBuilderFactory factory,
                             GitScmConfig gitScmConfig,
                             HookService hookService) {
        deployer = new RepoDeployer(factory, gitScmConfig, hookService);
    }

    @Override
    public void postReceive(RepositoryHookContext context, Collection<RefChange> refChanges) {
        try {
            deployer.deploy(context.getRepository());
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
}
