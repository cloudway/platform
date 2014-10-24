#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <limits.h>
#include <pwd.h>
#include <grp.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <syslog.h>

#define PAM_SM_AUTH
#define PAM_SM_SESSION

#include <security/pam_modules.h>
#include <security/_pam_macros.h>
#include <security/pam_modutil.h>
#include <security/pam_ext.h>

#include <selinux/selinux.h>
#include <selinux/context.h>
#include <selinux/get_default_type.h>

static security_context_t user_context = NULL;
static security_context_t prev_user_context = NULL;

PAM_EXTERN int
pam_sm_authenticate(pam_handle_t *pamh, int flags, int argc, const char **argv)
{
    /* Fail by default. */
    return PAM_AUTH_ERR;
}

PAM_EXTERN int
pam_sm_setcred(pam_handle_t *pamh, int flags, int argc, const char **argv)
{
    return PAM_SUCCESS;
}

static void get_mcs_level(int uid, security_context_t *con)
{
    int setsize = 1023;
    int tier = setsize;
    int ord = uid;

    if (uid < 1 || uid  > 523776) {
        return;
    }

    while (ord > tier) {
        ord = ord - tier;
        tier -= 1;
    }
    tier = setsize - tier;
    ord = ord + tier;
    asprintf(con, "unconfined_u:system_r:cloudway_t:s0:c%d,c%d", tier, ord);
}

static int is_on_list(char * const *list, const char *member)
{
    while (list && *list) {
        if (strcmp(*list, member) == 0)
            return 1;
        list++;
    }
    return 0;
}

static int cloudway_domain(pam_handle_t *pamh, struct passwd *pw)
{
    struct group *grp;
    security_context_t secontext;
    context_t parsed_context;
    char *comp_context = "cloudway_var_lib_t";
    int selength;
    int cmpval = -1;

    if (!pw->pw_uid)
        return 0;

    if (strlen(pw->pw_dir) != 0) {
        selength = getfilecon(pw->pw_dir, &secontext);
        if (selength > 0) {
            parsed_context = context_new(secontext);
            cmpval = strcmp(context_type_get(parsed_context), comp_context);
            context_free(parsed_context);
            freecon(secontext);
        }
    }
    if (cmpval != 0) {
        return 0;
    }

    /* make sure the user is not in the root group */
    if ((grp = pam_modutil_getgrnam(pamh, "wheel")) == NULL)
        grp = pam_modutil_getgrgid(pamh, 0);
    return !grp || (pw->pw_gid != grp->gr_gid && !is_on_list(grp->gr_mem, pw->pw_name));
}

PAM_EXTERN int
pam_sm_open_session(pam_handle_t *pamh, int flags, int argc, const char **argv)
{
    int i;
    int debug = 0, close_session=0;
    int status = PAM_SUCCESS;
    const void *void_username;
    const char *username;
    struct passwd *pw;

    for (i = 0; i < argc; i++) {
        if (strcmp(argv[i], "debug") == 0) {
            debug = 1;
        }
        if (strcmp(argv[i], "close") == 0) {
            close_session = 1;
        }
    }

    if (close_session) {
        return PAM_SUCCESS;
    }

    if (is_selinux_enabled() <= 0) {
        return PAM_SUCCESS;
    }

    if (debug) {
        pam_syslog(pamh, LOG_NOTICE, "Open Session");
    }

    if (pam_get_item(pamh, PAM_USER, &void_username) != PAM_SUCCESS || !void_username) {
        return PAM_USER_UNKNOWN;
    }

    username = void_username;
    pw = pam_modutil_getpwnam(pamh, username);
    if (!pw) {
        pam_syslog(pamh, LOG_ERR, "Unable to find uid for user %s", username);
        return -1;
    }

    if (cloudway_domain(pamh, pw)) {
        get_mcs_level(pw->pw_uid, &user_context);
    } else {
        user_context = strdup("unconfined_u:unconfined_r:unconfined_t:s0-s0:c0.c1023");
    }
    if (!user_context) {
        pam_syslog(pamh, LOG_ERR, "Unable to generate user context for user %s: %s",
                   username, strerror(errno));
        return -1;
    }

    if (getexeccon(&prev_user_context) < 0) {
        prev_user_context = NULL;
    }

    if (setexeccon(user_context)) {
        pam_syslog(pamh, LOG_ERR, "Error! Unable to set %s executable context %s. %s",
                   username, user_context, strerror(errno));
        if (security_getenforce() == 1) {
            status = PAM_AUTH_ERR;
        }
    } else if (debug) {
        pam_syslog(pamh, LOG_NOTICE, "Set %s security context to %s", username, user_context);
    }

    freecon(user_context);
    return status;
}

PAM_EXTERN int
pam_sm_close_session(pam_handle_t *pamh, int flags, int argc, const char **argv)
{
    int i;
    int debug = 0, open_session = 0;
    int status = PAM_SUCCESS;

    if (is_selinux_enabled() <= 0) {
        return PAM_SUCCESS;
    }

    for (i = 0; i < argc; i++) {
        if (strcmp(argv[i], "debug") == 0) {
            debug = 1;
        }
        if (strcmp(argv[i], "open") == 0) {
            open_session = 1;
        }
    }

    if (open_session) {
        return PAM_SUCCESS;
    }

    if (debug) {
        pam_syslog(pamh, LOG_NOTICE, "Close session");
    }

    if (setexeccon(prev_user_context)) {
        pam_syslog(pamh, LOG_ERR, "Unable to restore executable context %s: %s",
                   prev_user_context ? prev_user_context : "", strerror(errno));
        if (security_getenforce() == 1) {
            status = PAM_AUTH_ERR;
        }
    } else if (debug) {
        pam_syslog(pamh, LOG_NOTICE, "Executable context back to original %s",
                   prev_user_context ? prev_user_context : "");
    }

    if (prev_user_context) {
        freecon(prev_user_context);
        prev_user_context = NULL;
    }

    return status;
}