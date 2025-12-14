const test = comp => a => b => {
  const $0 = comp(a)(b);
  return $0 === "LT" || $0 === "GT" || $0 !== "EQ";
};
export {test};
