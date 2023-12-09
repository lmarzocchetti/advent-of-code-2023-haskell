use std::iter::*;

fn parse(file: &String) -> Vec<Vec<i32>> {
    let mut ret: Vec<Vec<&str>> = file.split("\n").into_iter()
        .map(|x| {x.split(" ").collect()})
        .collect();
    
    drop(ret.remove(ret.len() - 1));

    let ret_1: Vec<Vec<i32>> = ret.iter()
        .map(|x| {
            x
            .iter()
            .map(|y| {
                y.parse::<i32>().unwrap()
            })
            .collect()
        })
        .collect();

    return ret_1;
}

fn calculate_line(line: &Vec<i32>) -> Vec<Vec<i32>> {
    let mut ret: Vec<Vec<i32>> = vec![line.clone()];
    let mut lcp = line.clone();
    let mut calc: Vec<i32> = vec![1];

    while !calc.iter().all(|x| x.eq(&0)) {
        calc = vec![];
        let mut init: i32 = lcp[0];
        lcp.remove(0);

        for i in &lcp {
            calc.push(i - init);
            init = *i;
        }

        lcp = calc.clone();

        ret.push(calc.clone());
    }

    return ret;
}

fn calculate_number_part1(history: Vec<Vec<i32>>) -> i32 {
    let mut reversed: Vec<Vec<i32>> = history.clone();
    reversed.reverse();

    let mut res: i32 = 0;

    for i in reversed {
        res = res + i.last().unwrap();
    }
    
    res
}

fn calculate_number_part2(history: Vec<Vec<i32>>) -> i32 {
    let mut reversed: Vec<Vec<i32>> = history.clone();
    reversed.reverse();

    let mut res: i32 = 0;

    for i in reversed {
        res = i.first().unwrap() - res;
    }
    
    res
}

fn main() {
    let file = std::fs::read_to_string("input.txt").unwrap();
    let ret = parse(&file);
    let a: Vec<Vec<Vec<i32>>> = ret.iter()
        .map(|x| calculate_line(x))
        .collect();

    let res: i32 = a.iter()
        .map(|x| calculate_number_part1(x.to_owned()))
        .sum();

    println!("Part 1: {}", res);

    let res: i32 = a.iter()
        .map(|x| calculate_number_part2(x.to_owned()))
        .sum();

    println!("Part 2: {}", res);
}
