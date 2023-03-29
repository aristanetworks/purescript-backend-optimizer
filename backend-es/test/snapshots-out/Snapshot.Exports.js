const $Data_TypeAndCtor = _1 => ({tag: "Data_TypeAndCtor", _1});
const Newtype_TypeAndCtor = x => x;
const Data_TypeAndCtor = value0 => $Data_TypeAndCtor(value0);
const useNewtype_Used_TypeOnly = v => v;
const useData_Used_TypeOnly = v => v._1;
const classExported = dict => dict.classExported;
export {$Data_TypeAndCtor, Data_TypeAndCtor, Newtype_TypeAndCtor, classExported, useData_Used_TypeOnly, useNewtype_Used_TypeOnly};
