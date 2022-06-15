const average = arr => arr.reduce((a, b) => a + b) / arr.length;

const benchmark = name => runs => f => () => {
  if (!global.gc) {
    console.log('Garbage collection is not exposed, run with --expose-gc');
    return;
  }

  let counter = runs;
  let timing = [];
  let memory = [];

  while (counter > 0) {
    global.gc();

    const start = process.hrtime();

    f({});

    const end = process.hrtime(start);
    const memoryUsage = process.memoryUsage().rss / 1000000;
    const ms = (end[0]* 1000000000 + end[1]) / 1000000;

    timing.push(ms);
    memory.push(memoryUsage)

    counter--;
  }

  console.log(`${name}: averages over ${runs} runs:`)
  console.log(`\ttime: ${average(timing)} ms`);
  console.log(`\tresident set size: ${average(memory)} mb`);
}

export { benchmark };
