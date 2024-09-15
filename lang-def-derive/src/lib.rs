use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{spanned::Spanned, visit_mut::VisitMut, *};

#[proc_macro_derive(MemSerializable)]
pub fn derive_mem_serializable(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let ident = &input.ident;
    let (_, ty_generics, where_clause) = input.generics.split_for_impl();

    let mut adjusted_generics = input.generics.clone();
    let pos = get_or_add_position_param(&mut adjusted_generics);
    let (impl_generics, _, _) = adjusted_generics.split_for_impl();

    let mut serialized = input.clone();
    serialized.ident = Ident::new(&(ident.to_string() + "Serialized"), ident.span());
    let serialized_ident = &serialized.ident;
    if make_data_serialized(&mut serialized.data, &pos) {
        serialized.generics = adjusted_generics.clone();
        serialized.generics.params = serialized
            .generics
            .params
            .into_pairs()
            .filter(|pair| !matches!(pair.value(), GenericParam::Lifetime(_)))
            .collect();
    }
    let (_, serialized_ty_generics, _) = serialized.generics.split_for_impl();

    let mut expanded = quote! {
        #[derive(Clone, PartialEq, Debug)]
        #[automatically_derived]
        #serialized
    };

    if !matches!(input.data, Data::Union(_)) {
        let serialize_body = construct_serialize_body(&input.data, ident, serialized_ident);
        let deserialize_body = construct_deserialize_body(&input.data, ident, serialized_ident);

        expanded.extend(quote! {
            #[automatically_derived]
            impl #impl_generics ::lang_def::mem_serializable::MemSerializable<#pos> for #ident #ty_generics #where_clause {
                type Serialized = #serialized_ident #serialized_ty_generics;

                fn serialize(&self, relative_to: &#pos) -> Self::Serialized {
                    #serialize_body
                }

                fn deserialize(serialized: &Self::Serialized, relative_to: &#pos) -> Self {
                    #deserialize_body
                }
            }
        });
    }

    expanded.into()
}

fn make_data_serialized(data: &mut Data, pos: &Ident) -> bool {
    match data {
        Data::Struct(struct_data) => make_fields_serialized(&mut struct_data.fields, &pos),
        Data::Enum(enum_data) => {
            let mut modified = false;
            for variant in &mut enum_data.variants {
                modified |= make_fields_serialized(&mut variant.fields, &pos);
            }
            modified
        }
        Data::Union(union_data) => make_named_fields_serialized(&mut union_data.fields, &pos),
    }
}

fn make_fields_serialized(fields: &mut Fields, pos: &Ident) -> bool {
    let mut modified = false;
    for field in fields {
        modified |= make_field_serialized(field, pos);
    }
    modified
}

fn make_named_fields_serialized(fields: &mut FieldsNamed, pos: &Ident) -> bool {
    let mut modified = false;
    for field in &mut fields.named {
        modified |= make_field_serialized(field, pos);
    }
    modified
}

fn make_field_serialized(field: &mut Field, pos: &Ident) -> bool {
    let mut ty = field.ty.clone();
    MakeLifetimesStatic.visit_type_mut(&mut ty);
    field.ty =
        parse_quote!(<#ty as ::lang_def::mem_serializable::MemSerializable<#pos>>::Serialized);
    true
}

fn get_or_add_position_param(generics: &mut Generics) -> Ident {
    for param in &generics.params {
        if let GenericParam::Type(type_param) = param {
            for bound in &type_param.bounds {
                if let TypeParamBound::Trait(trait_bound) = bound {
                    if let Some(last_segment) = trait_bound.path.segments.last() {
                        if last_segment.ident == "Position" {
                            return type_param.ident.clone();
                        }
                    }
                }
            }
        }
    }

    let ident = Ident::new("Pos", Span::call_site());
    generics.params.push(GenericParam::Type(TypeParam {
        attrs: Vec::new(),
        ident: ident.clone(),
        colon_token: Default::default(),
        bounds: parse_quote!(::lang_def::parser::Position),
        eq_token: None,
        default: None,
    }));
    if generics.lt_token.is_none() {
        generics.lt_token = Some(Default::default());
    }
    if generics.gt_token.is_none() {
        generics.gt_token = Some(Default::default());
    }
    ident
}

fn construct_serialize_body(data: &Data, ident: &Ident, serialized_ident: &Ident) -> TokenStream {
    match data {
        Data::Struct(struct_data) => {
            let members = struct_data.fields.members();
            quote! {
                #serialized_ident {
                    #(#members: self.#members.serialize(relative_to)),*
                }
            }
        }
        Data::Enum(enum_data) => {
            let mut match_body = TokenStream::new();
            for variant in &enum_data.variants {
                let variant_ident = &variant.ident;
                let variant_members_1 = variant.fields.members();
                let variant_members_2 = variant_members_1.clone();
                let variant_member_idents_1 = variant_members_1.clone().map(make_member_ident);
                let variant_member_idents_2 = variant_member_idents_1.clone();
                match_body.extend(quote! {
                    #ident::#variant_ident {
                        #(#variant_members_1: #variant_member_idents_1),*
                    } => #serialized_ident::#variant_ident {
                        #(#variant_members_2: #variant_member_idents_2.serialize(relative_to)),*
                    },
                });
            }
            quote! {
                match self {
                    #match_body
                }
            }
        }
        Data::Union(_) => panic!("generic union serialization not possible"),
    }
}

fn construct_deserialize_body(data: &Data, ident: &Ident, serialized_ident: &Ident) -> TokenStream {
    match data {
        Data::Struct(struct_data) => {
            let members = struct_data.fields.members();
            quote! {
                #ident {
                    #(#members: <_>::deserialize(&serialized.#members, relative_to)),*
                }
            }
        }
        Data::Enum(enum_data) => {
            let mut match_body = TokenStream::new();
            for variant in &enum_data.variants {
                let variant_ident = &variant.ident;
                let variant_members_1 = variant.fields.members();
                let variant_members_2 = variant_members_1.clone();
                let variant_member_idents_1 = variant_members_1.clone().map(make_member_ident);
                let variant_member_idents_2 = variant_member_idents_1.clone();
                match_body.extend(quote! {
                    #serialized_ident::#variant_ident {
                        #(#variant_members_1: #variant_member_idents_1),*
                    } => #ident::#variant_ident {
                        #(#variant_members_2: <_>::deserialize(#variant_member_idents_2, relative_to)),*
                    },
                });
            }
            quote! {
                match serialized {
                    #match_body
                }
            }
        }
        Data::Union(_) => panic!("generic union deserialization not possible"),
    }
}

fn make_member_ident(member: Member) -> Ident {
    match member {
        Member::Named(ident) => ident,
        Member::Unnamed(index) => Ident::new(&format!("_{}", index.index), index.span()),
    }
}

struct MakeLifetimesStatic;

impl VisitMut for MakeLifetimesStatic {
    fn visit_lifetime_mut(&mut self, i: &mut Lifetime) {
        i.ident = Ident::new("static", Span::call_site());
    }
}
