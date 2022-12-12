export const loadModuleMainImpl = onError => onSuccess => path => () => {
  import(path).then(
    mod => onSuccess(mod.main)(),
    err => onError(err)()
  );
}
