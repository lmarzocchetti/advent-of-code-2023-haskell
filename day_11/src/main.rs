use itertools::Itertools;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
struct Pos {
    x: i64,
    y: i64
}

impl Pos {
    fn distance(&self, other: Pos) -> i64 {
        return i64::abs(self.x - other.x) + i64::abs(self.y - other.y);
    }

    fn distance_exp(&self, other: Pos, rowcol: (i64, i64)) -> i64 {
        let (y1, y2) = if self.y < other.y {(other.y, self.y)} else {(self.y, other.y)};
        let (x1, x2) = if self.x < other.x {(other.x, self.x)} else {(self.x, other.x)};

        return i64::abs((x1 + rowcol.1 * 99) - x2) + i64::abs((y1 + rowcol.0 * 99) - y2);
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
enum Block {
    STAR(Pos),
    EMPTY
}

#[derive(Debug)]
struct Galaxy {
    matrix: Vec<Vec<Block>>
}

impl Galaxy {
    fn new(mat: Vec<Vec<Block>>) -> Self {
        return Galaxy{matrix: mat};
    }

    fn all_couples(&self) -> Vec<(Block, Block)> {
        let stars: Vec<&Block> = self.matrix.iter()
            .flatten().filter(|x| {
                match x {
                    Block::EMPTY => false,
                    _ => true
                }
            }).collect::<Vec<&Block>>();
        
        return stars.iter().copied().permutations(2).unique().into_iter()
            .map(|x| {
                assert!(x.len() == 2);
                return (x[0].to_owned(), x[1].to_owned());
            }).collect::<Vec<(Block, Block)>>();
    }

    fn isEmptyRow(&self, row: i64) -> bool {
        return self.matrix.get(row as usize).unwrap().iter().all(|x| {
            return match x {
                Block::EMPTY => true,
                _ => false
            }
        });
    }

    fn isEmptyCol(&self, col: i64) -> bool{
        return Galaxy::isEmptyRow(&Galaxy::new(transpose(self.matrix.clone())), col);
    }

    fn countEmptyRowsColsBet(&self, first: &Pos, second: &Pos) -> (i64, i64) {
        let (y1, y2) = if first.y > second.y {(second.y, first.y)} else {(first.y, second.y)};
        let (x1, x2) = if first.x > second.x {(second.x, first.x)} else {(first.x, second.x)};
        assert!(y1 <= y2);
        assert!(x1 <= x2);
        
        let mut r = 0;
        let mut c = 0;

        for row in y1..y2 {
            if self.isEmptyRow(row) {
                r += 1;
            }
        }

        for col in x1..x2 {
            if self.isEmptyCol(col) {
                c += 1;
            }
        }
        
        return (r, c);
    }
}

fn transpose<T>(mat: Vec<Vec<T>>) -> Vec<Vec<T>> {
    let num_cols = mat.first().unwrap().len();
    let mut row_iters: Vec<_> = mat.into_iter().map(Vec::into_iter).collect();
    
    let out: Vec<Vec<_>> = (0..num_cols)
    .map(|_| row_iters.iter_mut().map(|it| it.next().unwrap()).collect())
    .collect();

    return out;

}

fn expandGalaxy(file: &str, rep: usize) -> Vec<String> {
    // Add rows and return to entire file String
    let exp_x: String = file.lines().into_iter()
        .map(|x| {
            if !x.contains('#') {
                return (x.to_owned() + "\n").repeat(rep);
            }
            return x.to_owned() + "\n";
        }).fold(String::from(""), |x: String, y: String| x.to_owned() + y.as_str());
        
    // Resplit in lines to add the new lines created
    let ret: Vec<Vec<u8>> = exp_x.lines().into_iter()
        .map(|x| {
            return x.bytes().into_iter()
                .map(|y| y).collect();
        }).collect::<Vec<Vec<u8>>>();
    
    // Transpose add new columns and return to entire file String
    let exp_y: String = transpose(ret).iter()
        .map(|x| {
            return String::from_utf8(x.to_owned()).unwrap();
        })
        .map(|x| {
            if !x.contains('#') {
                return (x.as_str().to_owned() + "\n").repeat(rep);
            }
            return x.to_owned() + "\n";
        }).fold(String::from(""), |x: String, y: String| x.to_owned() + y.as_str());
    
    // Resplit in lines to add the new lines created
    let out: Vec<Vec<u8>> = exp_y.lines().into_iter()
    .map(|x| {
        return x.bytes().into_iter()
            .map(|y| y).collect();
    }).collect::<Vec<Vec<u8>>>();

    // Retranspose back the matrix and create Vec<String>
    return transpose(out).iter()
        .map(|x| {
            return String::from_utf8(x.to_owned()).unwrap();
        }).collect();
}

fn parse(expanded: Vec<String>) -> Galaxy {
    let mat: Vec<Vec<Block>> = expanded.iter().enumerate()
        .map(|(ix, x)| {
            return x.bytes().enumerate()
                .map(|(iy, y)| {
                    match y {
                        b'.' => Block::EMPTY,
                        b'#' => Block::STAR(Pos{x:ix as i64, y:iy as i64}),
                        _ => panic!("Trovato blocco estraneo!")
                    }
            }).collect();
        }).collect::<Vec<Vec<Block>>>();
    
    return Galaxy::new(mat);
}

fn part_1(file: &str) {
    let expanded = expandGalaxy(file, 2);
    let galaxy = parse(expanded);
    let couples = galaxy.all_couples();
    let res: i64 = couples.iter()
        .map(|x| {
            match x {
                (Block::STAR(a), Block::STAR(b)) => a.distance(b.clone()),
                _ => panic!("Unreachable!")
            }
        }).sum();
    
        println!("Part 1: {:?}", res/2);
}

fn grid_distance(x1: i64, y1: i64, x2: i64, y2: i64) -> i64 {
    (x2 - x1).abs() + (y2 - y1).abs()
}

fn part_2(input: &str) {
    const EXPAND: i64 = 999_999;
    let width = input.lines().next().unwrap().len();
    let height = input.lines().count();
    let lines: Vec<String> = input.lines().map(|line| line.to_string()).collect();

    //map galaxies
    let mut coords = lines
        .iter()
        .enumerate()
        .flat_map(|(y, ln)| {
            ln.char_indices()
                .filter(|&(_, c)| c == '#')
                .map(move |(x, _)| Pos {
                    x: x as i64,
                    y: y as i64,
                })
                .collect::<Vec<Pos>>()
        })
        .collect::<Vec<Pos>>();

    let x_gallaxy_coords = coords.iter().map(|c| c.x).collect::<Vec<i64>>();
    let y_galaxy_coords = coords.iter().map(|c| c.y).collect::<Vec<i64>>();

    //compute expansions x,y coord
    let x_expanded = (0..width)
        .filter_map(|y| i64::try_from(y).ok())
        .filter(|y| !x_gallaxy_coords.contains(y))
        .collect::<Vec<i64>>();
    let y_expanded = (0..height)
        .filter_map(|y| i64::try_from(y).ok())
        .filter(|y| !y_galaxy_coords.contains(y))
        .collect::<Vec<i64>>();

    let x_gt_than = |x: i64| x_expanded.iter().filter(|&&x_expand| x > x_expand).count() as i64;
    let y_gt_than = |y: i64| y_expanded.iter().filter(|&&y_expand| y > y_expand).count() as i64;

    //mutate coord positions
    coords.iter_mut().for_each(|coord| {
        coord.x += x_gt_than(coord.x) * EXPAND;
        coord.y += y_gt_than(coord.y) * EXPAND;
    });

    let mut sum = 0;
    for i in 0..coords.len() {
        for j in i + 1..coords.len() {
            let (g1, g2) = (&coords[i], &coords[j]);
            sum += grid_distance(g1.x as i64, g1.y as i64, g2.x as i64, g2.y as i64);
        }
    }
    println!("Part 2: {sum}");
}

fn main() {
    let file = std::fs::read_to_string("input.txt").unwrap();
    part_1(&file);
    part_2(&file);
}
