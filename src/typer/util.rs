use crate::tast;

pub fn generate_qvars_from_generics(generics: &[String]) -> Vec<tast::Type> {
    let mut params = vec![];
    for (i, gvar) in generics.iter().enumerate() {
        for p in params.iter() {
            if let tast::Type::Param { index: _, name } = p {
                if name == gvar {
                    panic!("duplicate generic name");
                }
            }
        }
        let ty = tast::Type::Param {
            index: i as u32,
            name: gvar.into(),
        };
        params.push(ty);
    }
    params
}
