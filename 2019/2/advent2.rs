use std::fs;

fn read_input(f: &str) -> Vec<usize> {
    let input = fs::read_to_string(f).expect("can't read input file");
    input.split(",").map(|x| x.parse().unwrap()).collect()
}


fn run(input: &[usize]) -> Vec<usize> {
    let mut mem: Vec<usize> = input.into();
    let mut pointer = 0;
    while pointer < mem.len() {
        let op = mem[pointer];
        match op {
            1 | 2  => {
                let (x, y, z) = (mem[pointer + 1], mem[pointer + 2], mem[pointer + 3]);
                let (xv, yv) = (mem[x], mem[y]);
                if op == 1 {
                    mem[z] = xv + yv
                } else if op == 2{
                    mem[z] = xv * yv
                }
            }
            99 | _ => return mem
        }
        pointer += 4
    }
    mem
}


fn solve(input: &[usize]) -> usize {
    let mut input: Vec<usize> = input.into();
    input[1] = 12;
    input[2] = 2;
    run(&input)[0]
}


fn solve2(input: &[usize]) -> usize {
    let mut input: Vec<usize> = input.into();
    for x in 0..100 {
        for y in 0..100 {
            input[1] = x;
            input[2] = y;
            if run(&input)[0] == 19690720 {
                return x * 100 + y
            }
        }
    }
    return 0
}


fn main () {
    let input = read_input("input.txt");
    println!("{:?}", solve(&input));
    println!("{:?}", solve2(&input));
}
