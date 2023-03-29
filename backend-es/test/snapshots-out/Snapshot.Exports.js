const $DataTypeAndCtor = _1 => ({tag: "DataTypeAndCtor", _1});
const NewtypeTypeAndCtor = x => x;
const DataTypeAndCtor = value0 => $DataTypeAndCtor(value0);
const useNewtypeTypeOnly = v => v;
const useDataTypeOnlyCtorNameChange = v => v._1;
const useDataTypeOnly = v => v._1;
const classExportedMember = dict => dict.classExportedMember;
export {$DataTypeAndCtor, DataTypeAndCtor, NewtypeTypeAndCtor, classExportedMember, useDataTypeOnly, useDataTypeOnlyCtorNameChange, useNewtypeTypeOnly};
