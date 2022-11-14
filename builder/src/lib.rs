// #![allow(dead_code)]
// #![allow(unused)]
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{
    parse_macro_input, Data, DeriveInput, Field, GenericArgument, Ident, PathArguments, Type,
};

fn get_fields(data: &Data) -> Vec<&Field> {
    match data {
        Data::Enum(_) => unimplemented!(),
        Data::Union(_) => unimplemented!(),
        Data::Struct(ref s) => {
            let mut vf: Vec<&Field> = vec![];
            for field in s.fields.iter() {
                vf.push(field);
            }
            vf
        }
    }
}

fn get_field_name(field: &Field) -> Ident {
    field.ident.clone().unwrap()
}

fn ty_inner_type<'a>(wrapper: &str, ty: &'a syn::Type) -> Option<&'a Type> {
    if let syn::Type::Path(ref p) = ty {
        if p.path.segments.len() != 1 || p.path.segments[0].ident != wrapper {
            return None;
        }

        if let PathArguments::AngleBracketed(ref inner_ty) = p.path.segments[0].arguments {
            if inner_ty.args.len() != 1 {
                return None;
            }

            let inner_ty = inner_ty.args.first().unwrap();
            if let GenericArgument::Type(ref t) = inner_ty {
                return Some(t);
            }
        }
    }
    None
}

///
/// Optionizes non-optional fields to allow builder pattern.
///
fn builder_struct_field(field: &Field) -> proc_macro2::TokenStream {
    let name = &field.ident;
    let ty = &field.ty;
    if ty_inner_type("Option", ty).is_some() {
        quote! {
            #name: #ty
        }
    } else {
        quote! {
            #name: Option<#ty>
        }
    }
}

fn build_field(field: &Field) -> proc_macro2::TokenStream {
    let name = &field.ident;
    if ty_inner_type("Option", &field.ty).is_some() {
        quote! {
            #name: self.#name.clone()
        }
    } else {
        quote! {
            #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
        }
    }
}

fn setter_method(field: &Field) -> proc_macro2::TokenStream {
    let name = &field.ident;
    let ty = &field.ty;
    if let Some(inner_ty) = ty_inner_type("Option", ty) {
        quote! {
            fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        }
    } else {
        quote! {
            fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        }
    }
}

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let main_name = &ast.ident;
    let builder_name = Ident::new(&format!("{}Builder", main_name), Span::call_site());

    let fields: Vec<&Field> = get_fields(&ast.data);
    let field_names: Vec<Ident> = fields.iter().map(|f| get_field_name(f)).collect();

    let builder_struct_fields = fields.iter().map(|f| builder_struct_field(f));
    let build_fields = fields.iter().map(|f| build_field(f));
    let setter_methods = fields.iter().map(|f| setter_method(f));

    let expanded = quote!(
    use std::default::Default;
    use std::error::Error;

    pub struct #builder_name {
        #(#builder_struct_fields,)*
    }

    impl Default for #builder_name {
        fn default() -> Self {
            Self {
                #(#field_names: None,)*
            }
        }
    }

    impl #builder_name {
        #(
        #setter_methods
        )*

        fn build(&self) -> Result<#main_name, Box<dyn Error>> {
            Ok(
            #main_name {
                #(#build_fields,)*
            }
            )
        }
    }

    impl #main_name {
        pub fn builder() -> #builder_name {
            #builder_name::default()
        }
    }
    );

    TokenStream::from(expanded)
}
