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
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import java.io.InputStream;

import com.atlassian.bitbucket.hook.HookService;
import com.atlassian.bitbucket.hook.repository.RepositoryHookService;
import com.atlassian.bitbucket.permission.Permission;
import com.atlassian.bitbucket.permission.PermissionValidationService;
import com.atlassian.bitbucket.repository.Repository;
import com.atlassian.bitbucket.repository.RepositoryService;
import com.atlassian.bitbucket.rest.util.ResourcePatterns;
import com.atlassian.bitbucket.scm.git.GitScmConfig;
import com.atlassian.bitbucket.scm.git.command.GitCommandBuilderFactory;
import com.cloudway.bitbucket.plugins.RepoDeployer;
import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.spi.resource.Singleton;

@Singleton
@Path(ResourcePatterns.REPOSITORY_URI)
@Consumes(MediaType.APPLICATION_JSON)
public class RepoDeployerRestResource {
    private final RepoDeployer deployer;
    private final RepositoryService repositoryService;
    private final PermissionValidationService validator;

    RepoDeployerRestResource(GitCommandBuilderFactory cmdFactory,
                             GitScmConfig gitScmConfig, HookService hookService,
                             RepositoryService repositoryService,
                             PermissionValidationService validator,
                             RepositoryHookService repositoryHookService) {
        this.deployer = new RepoDeployer(cmdFactory, gitScmConfig,
                                         hookService, repositoryHookService,
                                         repositoryService);
        this.repositoryService = repositoryService;
        this.validator = validator;
    }

    @GET
    @Path("/archive")
    @Produces("application/tar")
    public Response archive(@Context Repository repository) {
        validator.validateForRepository(repository, Permission.REPO_READ);

        try {
            InputStream entity = deployer.archive(repository);
            return Response.ok(entity).build();
        } catch (Exception ex) {
            return Response.serverError().build();
        }
    }

    @POST
    @Path("/deploy")
    public Response deploy(@Context Repository repository) {
        validator.validateForRepository(repository, Permission.REPO_READ);

        try {
            deployer.deploy(repository, false);
            return Response.ok().build();
        } catch (Exception ex) {
            return Response.serverError().build();
        }
    }

    @PUT
    @Path("/populate")
    @Consumes("application/tar")
    public Response populate(@Context Repository repository, InputStream payload) {
        validator.validateForRepository(repository, Permission.REPO_WRITE);

        if (repositoryService.isEmpty(repository)) {
            try {
                deployer.populate(repository, payload);
                return Response.noContent().build();
            } catch (Exception ex) {
                return Response.serverError().build();
            }
        } else {
            return Response.status(ClientResponse.Status.FORBIDDEN).build();
        }
    }

    @POST
    @Path("/populate")
    public Response populate(@Context Repository repository, @QueryParam("url") String url) {
        validator.validateForRepository(repository, Permission.REPO_WRITE);

        if (repositoryService.isEmpty(repository)) {
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

        if (repositoryService.isEmpty(repository)) {
            return Response.noContent().build();
        } else {
            return Response.status(Response.Status.FORBIDDEN).build();
        }
    }
}
