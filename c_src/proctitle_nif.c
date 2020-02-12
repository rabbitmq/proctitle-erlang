#include <stdbool.h>
#include <string.h>

#if defined(__linux__)
#include <sys/prctl.h>
#elif defined(__FreeBSD__)
#include <sys/types.h>
#include <unistd.h>
#endif

#include <erl_nif.h>

static ERL_NIF_TERM	atom_ok;
static ERL_NIF_TERM	atom_keep_exec;

static ERL_NIF_TERM	set_proctitle(ErlNifEnv *env, int argc,
			    const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM	reset_proctitle(ErlNifEnv *env, int argc,
			    const ERL_NIF_TERM argv[]);

#if defined(__FreeBSD__)
static int
is_keep_exec_true(ErlNifEnv *env, ERL_NIF_TERM options, bool *keep_exec)
{
	int ret;
	char keep_exec_value[10];
        ERL_NIF_TERM keep_exec_term;

        if (!enif_is_map(env, options)) {
		return -1;
        }

	ret = enif_get_map_value(env, options,
	    atom_keep_exec, &keep_exec_term);
        if (!ret) {
		*keep_exec = true;
		return 0;
        }

        if (!enif_is_atom(env, keep_exec_term)) {
		return -1;
        }

        ret = enif_get_atom(env, keep_exec_term,
	    keep_exec_value, (unsigned int)sizeof(keep_exec_value),
            ERL_NIF_LATIN1);
	if (ret == 0) {
		return -1;
        }

        if (strcmp(keep_exec_value, "true") == 0) {
		*keep_exec = true;
		return 0;
        } else if (strcmp(keep_exec_value, "false") == 0) {
		*keep_exec = false;
		return 0;
        } else {
		return -1;;
        }

}
#endif

#if defined(__linux__)
static ERL_NIF_TERM
set_proctitle(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	char *proctitle;
	ErlNifBinary proctitle_bin;

	if (!enif_inspect_binary(env, argv[0], &proctitle_bin)) {
		return enif_make_badarg(env);
	}

	proctitle = enif_alloc(proctitle_bin.size + 1);
	if (proctitle == NULL) {
		return enif_make_badarg(env);
	}

	memcpy(proctitle, proctitle_bin.data, proctitle_bin.size);
	proctitle[proctitle_bin.size] = '\0';

	/* FIXME: It seems ineffective. Perhaps because it is
	 * called in the context of a thread? */
	if (prctl(PR_SET_NAME, proctitle, NULL, NULL, NULL) != 0) {
		return enif_make_badarg(env);
	}

	return atom_ok;
}

static ERL_NIF_TERM
reset_proctitle(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	/* Unsupported. */

	return atom_ok;
}
#elif defined(__FreeBSD__)
static ERL_NIF_TERM
set_proctitle(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	int ret;
	bool keep_exec;
	ErlNifBinary proctitle_bin;

	if (!enif_inspect_binary(env, argv[0], &proctitle_bin)) {
		return enif_make_badarg(env);
	}

	ret = is_keep_exec_true(env, argv[1], &keep_exec);
	if (ret < 0) {
		return enif_make_badarg(env);
	}

	setproctitle(
	    keep_exec ? "%.*s" : "-%.*s",
	    (int)proctitle_bin.size, proctitle_bin.data);

	return atom_ok;
}

static ERL_NIF_TERM
reset_proctitle(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	setproctitle(NULL);

	return atom_ok;
}
#else
static ERL_NIF_TERM
set_proctitle(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	/* Unsupported. */

	return atom_ok;
}

static ERL_NIF_TERM
reset_proctitle(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	/* Unsupported. */

	return atom_ok;
}
#endif

static int
load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{

	atom_ok = enif_make_atom(env, "ok");
	atom_keep_exec = enif_make_atom(env, "keep_executable");

	*priv_data = NULL;

	return 0;
}

static ErlNifFunc nif_funcs[] = {
	{"set", 2, set_proctitle},
	{"reset", 0, reset_proctitle},
};

ERL_NIF_INIT(proctitle_nif, nif_funcs, load, NULL, NULL, NULL)
