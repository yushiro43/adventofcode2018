fn main() {
    let find_this = "165061";
    let mut recipes = "37".to_string();
    recipes.reserve(10_000_000);
    let mut elf1 = 0usize;
    let mut elf2 = 1usize;
    loop {
        if recipes.len() % 100000 == 0 && recipes.len() >= 200000
            && recipes.get(100010..).unwrap().find(find_this).is_some()
        {
            break;
        }
        let r1 = (recipes.as_bytes()[elf1] - 48) as usize;
        let r2 = (recipes.as_bytes()[elf2] - 48) as usize;
        recipes.push_str(&(r1 + r2).to_string());
        elf1 = (elf1 + r1 + 1) % recipes.len();
        elf2 = (elf2 + r2 + 1) % recipes.len();
    }

    println!("{}", recipes.find(find_this).unwrap());
}
