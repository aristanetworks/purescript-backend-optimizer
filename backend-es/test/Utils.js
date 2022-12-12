export const loadModuleMainImpl = onError => onSuccessMain => onSuccess => path => () => {
  import(path).then(
    mod => mod.main ? onSuccessMain(mod.main)() : onSuccess(),
    err => onError(err)()
  );
}
