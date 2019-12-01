use std::fs;

fn read_input(f: &str) -> Vec<i32> {
    let input = fs::read_to_string(f).expect("can't open file");
    input.lines().map(|x| x.parse().unwrap()).collect()
}


fn fuel(mass: &i32) -> i32 {
    mass / 3 - 2
}


fn solve(input: &[i32]) -> i32 {
    input.iter().fold(0, |acc, mass| acc + fuel(mass))
}


fn total_fuel(mass: &i32) -> i32 {
    let mut total = 0;
    let mut partial = fuel(mass);
    while partial > 0 {
        total += partial;
        partial = fuel(&partial)
    }
    total
}


fn solve2(input: &[i32]) -> i32 {
    input.iter().fold(0, |acc, mass| acc + total_fuel(mass))
}


fn main() {
    let input = read_input("input.txt");
    println!("{:?}", solve(&input));
    println!("{:?}", solve2(&input))
}
