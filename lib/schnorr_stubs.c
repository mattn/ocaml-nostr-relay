#include <string.h>

#include <secp256k1.h>
#include <secp256k1_schnorrsig.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

/* A verification-only context is thread safe once created. */
static secp256k1_context *verify_context(void) {
  static secp256k1_context *ctx = NULL;
  if (ctx == NULL) {
    ctx = secp256k1_context_create(SECP256K1_CONTEXT_VERIFY);
  }
  return ctx;
}

/* nostr_schnorr_verify sig64 msg32 pubkey32 : bool */
CAMLprim value nostr_schnorr_verify(value vsig, value vmsg, value vpub) {
  CAMLparam3(vsig, vmsg, vpub);
  unsigned char sig[64], msg[32], pub[32];
  secp256k1_context *ctx;
  secp256k1_xonly_pubkey pubkey;

  if (caml_string_length(vsig) != 64 || caml_string_length(vmsg) != 32 ||
      caml_string_length(vpub) != 32) {
    CAMLreturn(Val_false);
  }
  memcpy(sig, String_val(vsig), sizeof(sig));
  memcpy(msg, String_val(vmsg), sizeof(msg));
  memcpy(pub, String_val(vpub), sizeof(pub));

  ctx = verify_context();
  if (ctx == NULL) {
    CAMLreturn(Val_false);
  }
  if (!secp256k1_xonly_pubkey_parse(ctx, &pubkey, pub)) {
    CAMLreturn(Val_false);
  }
  if (!secp256k1_schnorrsig_verify(ctx, sig, msg,
#ifdef SECP256K1_SCHNORRSIG_EXTRAPARAMS_INIT
                                   sizeof(msg),
#endif
                                   &pubkey)) {
    CAMLreturn(Val_false);
  }
  CAMLreturn(Val_true);
}
