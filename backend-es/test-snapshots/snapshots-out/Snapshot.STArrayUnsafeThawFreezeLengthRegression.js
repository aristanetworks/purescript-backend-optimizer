const test = x => {
  const result = [x];
  result.push(12);
  result.push(result.length);
  return result;
};
export {test};
