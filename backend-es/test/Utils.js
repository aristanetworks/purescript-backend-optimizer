import { spawn } from "child_process";

export const loadModuleMainImpl = onError => onSuccessMain => onSuccess => path => () => {
  import(path).then(
    mod => mod.main ? onSuccessMain(mod.main)() : onSuccess(),
    err => onError(err)()
  );
}

export const spawnImpl = cmd => args => () => {
  return spawn(cmd, args, { shell: true, stdio: "inherit" });
}
