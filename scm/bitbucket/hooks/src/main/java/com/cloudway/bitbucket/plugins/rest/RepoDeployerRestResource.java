/**
 * Cloudway Platform
 * Copyright (c) 2012-2016 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.bitbucket.plugins.rest;

import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import java.io.InputStream;

import com.atlassian.bitbucket.permission.Permission;
import com.atlassian.bitbucket.permission.PermissionValidationService;
import com.atlassian.bitbucket.repository.Repository;
import com.atlassian.bitbucket.rest.util.ResourcePatterns;
import com.atlassian.bitbucket.scm.git.command.GitCommandBuilderFactory;
import com.cloudway.bitbucket.plugins.RepoDeployer;
import com.sun.jersey.spi.resource.Singleton;

@Consumes(MediaType.APPLICATION_JSON)
@Path(ResourcePatterns.REPOSITORY_URI)
@Singleton
public class RepoDeployerRestResource {
    private final RepoDeployer deployer;
    private final PermissionValidationService validator;

    RepoDeployerRestResource(GitCommandBuilderFactory gitCommandBuilderFactory,
                             PermissionValidationService validator) {
        this.deployer = new RepoDeployer(gitCommandBuilderFactory);
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
            deployer.deploy(repository);
            return Response.ok().build();
        } catch (Exception ex) {
            return Response.serverError().build();
        }
    }
}
