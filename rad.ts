import type { Task, Tasks } from "https://deno.land/x/rad/src/mod.ts";

const dayInts = Array.from(new Array(26)).map((_, i) => i + 1);

const clean: Task = `fd -H -I .merlin -x rm {}`;
const format: Task = {
  async fn({ sh }) {
    return Promise.all([
      sh(
        `fd -E _build ml . -x opam exec ocamlformat -- -i --enable-outside-detected-project`,
      ),
      sh(`deno --unstable -L debug fmt --ignore="_build"`)
    ]);
  },
};
const runAllDays: Task = dayInts.map((i) => `rad -l info run_day_${i}`).join(
  " && ",
);
export const tasks: Tasks = {
  ...{ c: clean, clean },
  ...{ f: format, format },
  ...(dayInts.reduce((acc, i) => {
    const run =
      `cp fixture.ml day${i}/ && cd day${i} && dune exec ./bin.exe ./input.txt`;
    return {
      ...acc,
      ...{
        [`run_day_${i}`]: run,
        [`rd${i}`]: run,
      },
    };
  }, {} as Record<string, Task>)),
  runAllDays,
};
