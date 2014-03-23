#include "erl_nif.h"
#include <atomic>
#include <string>

extern "C" {
    static ErlNifResourceType* robin_q_RESOURCE = NULL;

    typedef struct {
        ErlNifRWLock *lock;
        ErlNifEnv *env;
        const ERL_NIF_TERM *elements;
        std::atomic_uint index;
        int size;
    } robin_q_handle;

    // Prototypes
    static ERL_NIF_TERM robin_q_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM robin_q_next(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM robin_q_set(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    void do_set(robin_q_handle *handle, ERL_NIF_TERM value);

    static ErlNifFunc nif_funcs[] = {
        {"do_new", 1, robin_q_new},
        {"next", 1, robin_q_next},
        {"do_set", 2, robin_q_set}
    };

    static ERL_NIF_TERM robin_q_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
        robin_q_handle* handle = (robin_q_handle*)enif_alloc_resource(robin_q_RESOURCE,
                sizeof(robin_q_handle));

        handle->env         = enif_alloc_env();
        handle->lock        = enif_rwlock_create("robin_q");

        do_set(handle, argv[0]);

        ERL_NIF_TERM result = enif_make_resource(env, handle);
        enif_release_resource(handle);
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
    }


    static ERL_NIF_TERM robin_q_next(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
        robin_q_handle *handle = NULL;
        unsigned int index;
        ERL_NIF_TERM result;

        if (enif_get_resource(env, argv[0], robin_q_RESOURCE, (void**)&handle) == 0) {
            return enif_make_badarg(env);
        }

        // CRITICAL SECTION
        // No writing to handle->elements should occur here
        enif_rwlock_rlock(handle->lock);
        index   = (handle->index++) % handle->size;
        result  = enif_make_copy(env, handle->elements[index]);
        enif_rwlock_runlock(handle->lock);

        return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
    }

    static ERL_NIF_TERM robin_q_set(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        robin_q_handle *handle = NULL;
        if (enif_get_resource(env, argv[0], robin_q_RESOURCE, (void**)&handle) == 0) {
            return enif_make_badarg(env);
        }

        enif_rwlock_rwlock(handle->lock);
        enif_clear_env(handle->env);
        do_set(handle, argv[1]);
        enif_rwlock_rwunlock(handle->lock);

        return enif_make_atom(env, "ok");
    }

    void do_set(robin_q_handle *handle, ERL_NIF_TERM value) {
        const ERL_NIF_TERM *elements_tuple;
        int elements_size;
        ERL_NIF_TERM elements_term = enif_make_copy(handle->env, value);
        enif_get_tuple(handle->env, elements_term, &elements_size, &elements_tuple);

        handle->size        = elements_size;
        handle->elements    = elements_tuple;
        handle->index       = 0;
    }

    static void robin_q_resource_cleanup(ErlNifEnv* env, void* arg) {
        robin_q_handle *handle = (robin_q_handle*)arg;
        handle->elements = NULL;
        enif_free_env(handle->env);
    }

    static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
        ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
        ErlNifResourceType* rt = enif_open_resource_type(env, NULL,
                "robin_q_resource",
                &robin_q_resource_cleanup,
                flags, NULL);
        if (rt == NULL)
            return -1;

        robin_q_RESOURCE = rt;

        return 0;
    }

    ERL_NIF_INIT(robin_q, nif_funcs, &on_load, NULL, NULL, NULL);
};
