import * as fs from 'fs'
import * as rimraf from 'rimraf'

export const rmRecursive = (path) => () => {
    rimraf.sync(path);
}
export const copyFolderSync = (from) => (to) => () => {
    fs.mkdirSync(to);
    fs.readdirSync(from).forEach(element => {
        if (fs.lstatSync(path.join(from, element)).isFile()) {
            fs.copyFileSync(path.join(from, element), path.join(to, element));
        } else {
            copyFolderSync(path.join(from, element), path.join(to, element));
        }
    });
}