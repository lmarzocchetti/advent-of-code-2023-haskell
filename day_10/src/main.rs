use std::collections::HashSet;

struct Grid {
    data: Box<[u8]>,
    res_pos: usize
}

impl Grid {
    fn from_str(s: &str) -> Self {
        let mut lines = s.lines().peekable();
        let line_len = lines.peek().map_or(0, |l| l.len());
        Self { 
            data: lines.flat_map(str::as_bytes).copied().collect(),
            res_pos: line_len }
    }

    fn find_position(&mut self) -> Option<(usize, u8)> {
        let my_pos = self.data.iter().position(|&x| x == b'S')?;
        let direction = if let Some(b'|' | b'7' | b'F') = my_pos.checked_sub(self.res_pos).and_then(|p| self.data.get(p)) {
            0
        } else if let Some(b'|' | b'L' | b'J') = self.data.get(my_pos + self.res_pos) {
            1
        } else if let Some(b'-' | b'L' | b'F') = my_pos.checked_sub(1).and_then(|p| self.data.get(p)) {
            2
        } else if let Some(b'-' | b'7' | b'J') = self.data.get(my_pos + 1) {
            2
        } else {
            return None;
        };
        Some((my_pos, direction))
    }

    fn do_loop(&self, p:usize, dir:u8) -> (usize, HashSet<usize>) {
        let go = self.res_pos as isize;
        let ap = [-go, go, -1, 1];
        let mut av = (p, dir);
        let mut set = HashSet::new();
        set.insert(p);
        loop {
            let Some(np) = av.0.checked_add_signed(ap[av.1 as usize]) else {
                break;
            };
            let Some(nextdir) = self.data.get(np)
                .and_then(|&pb| next_dir(av.1, pb)) else {
                    break;
                };
            if set.insert(np) {
                av = (np, nextdir);
            } else {
                break;
            }
        }
        (set.len()/2, set)
    }
}

const fn next_dir(dbit: u8, pb: u8) -> Option<u8> {
    Some(match (dbit, pb) {
        (0, b'|') | (2, b'L') | (3, b'J') => 0,
        (1, b'|') | (2, b'F') | (3, b'7') => 1,
        (0, b'7') | (1, b'J') | (2, b'-') => 2,
        (0, b'F') | (1, b'L') | (3, b'-') => 3,
        _ => return None,
    })
}

fn main() {
    let file = std::fs::read_to_string("input.txt").unwrap();
    let mut grid = Grid::from_str(&file);
    let (my_pos, direction) = grid.find_position().unwrap();
    let (steps, set) = grid.do_loop(my_pos, direction);
    println!("Part 1: {}", steps);

    let (mut total, mut inside) = (0, false);
    let valid = direction == 0;
    for p in 0..grid.data.len() {
        if set.contains(&p) {
            match grid.data[p] {
                b'|' | b'J' | b'L' => inside = !inside,
                b'S' if valid => inside = !inside,
                _ => {}
            }
        } else {
            total += inside as u32;
        }
        if p % grid.res_pos == grid.res_pos - 1 {
            inside = false;
        }
    }

    println!("Part 2: {}", total);
}
