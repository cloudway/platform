/**
 * Cloudway Platform
 * Copyright (c) 2012-2016 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.bitbucket.plugins.rest;

import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.HEAD;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.StreamingOutput;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import com.atlassian.bitbucket.hook.HookService;
import com.atlassian.bitbucket.hook.repository.RepositoryHookService;
import com.atlassian.bitbucket.permission.Permission;
import com.atlassian.bitbucket.permission.PermissionValidationService;
import com.atlassian.bitbucket.repository.Ref;
import com.atlassian.bitbucket.repository.RefService;
import com.atlassian.bitbucket.repository.Repository;
import com.atlassian.bitbucket.repository.RepositoryService;
import com.atlassian.bitbucket.rest.util.ResourcePatterns;
import com.atlassian.bitbucket.scm.git.GitScmConfig;
import com.atlassian.bitbucket.scm.git.command.GitCommandBuilderFactory;
import com.cloudway.bitbucket.plugins.RepoDeployer;
import com.sun.jersey.spi.resource.Singleton;

@Singleton
@Path(ResourcePatterns.REPOSITORY_URI)
@Consumes(MediaType.APPLICATION_JSON)
public class RepoDeployerRestResource {
    private final RepoDeployer deployer;
    private final RepositoryService repoService;
    private final PermissionValidationService validator;

    RepoDeployerRestResource(GitCommandBuilderFactory cmdFactory,
                             GitScmConfig gitScmConfig,
                             HookService hookService,
                             RepositoryHookService repoHookService,
                             RepositoryService repoService,
                             RefService refService,
                             PermissionValidationService validator) {
        this.deployer = new RepoDeployer(cmdFactory, gitScmConfig,
                                         hookService, repoHookService,
                                         repoService, refService);
        this.repoService = repoService;
        this.validator = validator;
    }

    @GET
    @Path("/archive")
    @Produces("application/tar")
    public Response archive(@Context Repository repository) {
        validator.validateForRepository(repository, Permission.REPO_READ);

        try {
            Ref ref = deployer.getDeploymentBranch(repository);
            InputStream entity = deployer.archive(repository, ref);
            return Response.ok(entity).build();
        } catch (Exception ex) {
            return Response.serverError().build();
        }
    }

    @POST
    @Path("/deploy")
    public Response deploy(@Context final Repository repository, @QueryParam("branch") final String branch) {
        validator.validateForRepository(repository, Permission.REPO_READ);

        StreamingOutput stream = new StreamingOutput() {
            @Override
            public void write(OutputStream out) throws IOException {
                try {
                    if (branch != null && !branch.isEmpty()) {
                        deployer.setDeploymentBranch(repository, branch);
                    }

                    Ref ref = deployer.getDeploymentBranch(repository);
                    OutputStream stdout = new StdWriter(out, StdWriter.Stdout);
                    OutputStream stderr = new StdWriter(out, StdWriter.Stderr);
                    deployer.deploy(repository, ref, stdout, stderr);
                } catch (IOException ioe) {
                    throw ioe;
                } catch (Exception ex) {
                    throw new WebApplicationException(ex);
                }
            }
        };

        return Response.ok(stream).build();
    }

    @GET
    @Path("/settings")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getSettings(@Context Repository repository) {
        validator.validateForRepository(repository, Permission.REPO_READ);

        Ref ref = deployer.getDeploymentBranch(repository);
        BranchSettings settings = new BranchSettings(ref);
        return Response.ok(settings).build();
    }

    @PUT
    @Path("/populate")
    @Consumes("application/tar")
    public Response populate(@Context Repository repository, InputStream payload) {
        validator.validateForRepository(repository, Permission.REPO_WRITE);

        if (repoService.isEmpty(repository)) {
            try {
                deployer.populate(repository, payload);
                return Response.noContent().build();
            } catch (Exception ex) {
                return Response.serverError().build();
            }
        } else {
            return Response.status(Response.Status.FORBIDDEN).build();
        }
    }

    @POST
    @Path("/populate")
    public Response populate(@Context Repository repository, @QueryParam("url") String url) {
        validator.validateForRepository(repository, Permission.REPO_WRITE);

        if (repoService.isEmpty(repository)) {
            try {
                deployer.populate(repository, url);
                return Response.noContent().build();
            } catch (Exception ex) {
                return Response.serverError().build();
            }
        } else {
            return Response.status(Response.Status.FORBIDDEN).build();
        }
    }

    @HEAD
    @Path("/populate")
    public Response checkEmpty(@Context Repository repository) {
        validator.validateForRepository(repository, Permission.REPO_WRITE);

        if (repoService.isEmpty(repository)) {
            return Response.noContent().build();
        } else {
            return Response.status(Response.Status.FORBIDDEN).build();
        }
    }
}
