/**
 * Cloudway Platform
 * Copyright (c) 2012-2016 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.bitbucket.plugins;

import java.util.Collection;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.atlassian.bitbucket.hook.HookService;
import com.atlassian.bitbucket.hook.repository.AsyncPostReceiveRepositoryHook;
import com.atlassian.bitbucket.hook.repository.RepositoryHookContext;
import com.atlassian.bitbucket.hook.repository.RepositoryHookService;
import com.atlassian.bitbucket.repository.RefChange;
import com.atlassian.bitbucket.repository.Repository;
import com.atlassian.bitbucket.repository.RepositoryService;
import com.atlassian.bitbucket.scm.git.GitScmConfig;
import com.atlassian.bitbucket.scm.git.command.GitCommandBuilderFactory;

@SuppressWarnings("unused")
public class AsyncPostReceiveDeployer implements AsyncPostReceiveRepositoryHook {
    private final RepoDeployer deployer;

    private static final Logger logger = Logger.getLogger(RepoDeployer.class.getName());
    static {
        if (System.getenv("CLOUDWAY_DEBUG_HOOK") != null) {
            logger.setLevel(Level.FINE);
        }
    }

    AsyncPostReceiveDeployer(GitCommandBuilderFactory factory,
                             GitScmConfig gitScmConfig,
                             HookService hookService,
                             RepositoryHookService repositoryHookService,
                             RepositoryService repositoryService) {
        deployer = new RepoDeployer(factory, gitScmConfig,
                                    hookService, repositoryHookService,
                                    repositoryService);
    }

    @Override
    public void postReceive(RepositoryHookContext context, Collection<RefChange> refChanges) {
        if (masterBranchChanged(refChanges)) {
            try {
                Repository repo = context.getRepository();
                logger.fine("Push to deploy the repository " +
                            repo.getSlug().toLowerCase() + "-" +
                            repo.getProject().getKey().toLowerCase());
                deployer.deploy(context.getRepository());
            } catch (Exception ex) {
                logger.log(Level.SEVERE, "Push to deploy failed", ex);
            }
        }
    }

    private static boolean masterBranchChanged(Collection<RefChange> refChanges) {
        for (RefChange change : refChanges) {
            if ("refs/heads/master".equals(change.getRef().getId())) {
                return true;
            }
        }
        return false;
    }
}
