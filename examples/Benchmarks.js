const benchmark = name => runs => f => () => {
  if (!global.gc) {
    console.log('Garbage collection is not exposed, run with --expose-gc');
    return;
  }

  let counter = runs;
  let timing = 0;
  let memory = 0;

  while (counter > 0) {
    global.gc();

    const start = process.hrtime();

    f({});

    const end = process.hrtime(start);
    const memoryUsage = process.memoryUsage().rss / 1000000;
    const ms = (end[0]* 1000000000 + end[1]) / 1000000;

    timing += ms; 
    memory += memoryUsage;

    counter--;
  }

  console.log(`${name}: averages over ${runs} runs:`)
  console.log(`\ttime: ${timing / runs} ms`);
  console.log(`\tresident set size: ${memory / runs} mb`);
}

export { benchmark };
