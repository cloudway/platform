/**
 * Cloudway Platform
 * Copyright (c) 2012-2016 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.bitbucket.plugins.rest;

import com.atlassian.bitbucket.repository.Ref;
import org.codehaus.jackson.annotate.JsonProperty;
import org.codehaus.jackson.map.annotate.JsonSerialize;

@JsonSerialize
public class BranchSettings {
    @JsonProperty
    public String id;

    @JsonProperty
    public String displayId;

    @JsonProperty
    public String type;

    public BranchSettings(Ref ref) {
        this.id = ref.getId();
        this.displayId = ref.getDisplayId();
        this.type = ref.getType().toString();
    }
}
