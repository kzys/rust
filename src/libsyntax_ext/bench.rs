// Copyright 2013 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

/// The expansion from a test function to the appropriate test struct for libtest
/// Ideally, this code would be in libtest but for efficiency and error messages it lives here.

use syntax::ext::base::*;
use syntax::ext::build::AstBuilder;
use syntax::ext::hygiene::{self, Mark, SyntaxContext};
use syntax::ast;
use syntax::print::pprust;
use syntax::symbol::Symbol;
use syntax_pos::{DUMMY_SP, Span};
use syntax::codemap::{ExpnInfo, MacroAttribute, Spanned};
use syntax::tokenstream::TokenStream;

pub fn expand_bench(
    cx: &mut ExtCtxt,
    _sp: Span,
    _meta_item: &ast::MetaItem,
    item: &Annotatable,
    push: &mut dyn FnMut(Annotatable)
) {
    let item =
        if let Annotatable::Item(i) = item { i }
        else {
            cx.parse_sess.span_diagnostic.span_fatal(item.span(),
                "#[bench] attribute is only allowed on functions").raise();
        };

    if !has_bench_signature(item) {
        cx.parse_sess.span_diagnostic.span_err(item.span, "functions used as benches must have \
            signature `fn(&mut Bencher) -> impl Termination`");
        return;
    }

    let sp = {
        let mark = Mark::fresh(Mark::root());
        mark.set_expn_info(ExpnInfo {
            call_site: DUMMY_SP,
            def_site: None,
            format: MacroAttribute(Symbol::intern("test")),
            allow_internal_unstable: true,
            allow_internal_unsafe: false,
            local_inner_macros: false,
            edition: hygiene::default_edition(),
        });
        item.span.with_ctxt(SyntaxContext::empty().apply_mark(mark))
    };

    let self_id = cx.ident_of("self");
    let test_id = cx.ident_of("test");

    // creates self::test::$name
    let test_path = |name| {
        cx.path(sp, vec![self_id, test_id, cx.ident_of(name)])
    };

    // creates self::test::$name
    let should_panic_path = |name| {
        cx.path(sp, vec![self_id, test_id, cx.ident_of("ShouldPanic"), cx.ident_of(name)])
    };

    // creates $name: $expr
    let field = |name, expr| cx.field_imm(sp, cx.ident_of(name), expr);

    // A simple ident for a lambda
    let b = cx.ident_of("b");

    let mut test_const = cx.item(sp, cx.ident_of("TEST_CASE"),
        // #[test_case]
        vec![cx.attribute(sp, cx.meta_word(sp, Symbol::intern("test_case")))],
        // const $ident: self::test::TestDescAndFn =
        ast::ItemKind::Const(cx.ty(sp, ast::TyKind::Path(None, test_path("TestDescAndFn"))),
            // self::test::TestDescAndFn {
            cx.expr_struct(sp, test_path("TestDescAndFn"), vec![
                // desc: self::test::TestDesc
                field("desc", cx.expr_struct(sp, test_path("TestDesc"), vec![
                    // name: module_path!()
                    field("name", cx.expr_call(sp, cx.expr_path(test_path("StaticTestName")),
                        vec![
                            cx.expr(sp, ast::ExprKind::Mac(Spanned {
                                span: sp,
                                node: ast::Mac_ {
                                    path: cx.path_ident(sp, cx.ident_of("module_path")),
                                    delim: ast::MacDelimiter::Parenthesis,
                                    tts: TokenStream::empty().into(),
                                },
                            }))
                        ])),
                    // ignore: false
                    field("ignore", cx.expr_bool(sp, false)),
                    // allow_fail: false
                    field("allow_fail", cx.expr_bool(sp, false)),
                    // should_panic: self::test::ShouldPanic::No
                    field("should_panic", cx.expr_path(should_panic_path("No"))),
                ])),
                // },
                // testfn: self::test::StaticTestFn(
                field("testfn", cx.expr_call(sp, cx.expr_path(test_path("StaticBenchFn")), vec![
                    // |b| self::test::assert_test_result(
                    cx.lambda1(sp,
                        cx.expr_call(sp, cx.expr_path(test_path("assert_test_result")), vec![
                            // super::$test_fn(b)
                            cx.expr_call(sp,
                                cx.expr_path(cx.path(sp, vec![cx.ident_of("super"), item.ident])),
                                vec![cx.expr_ident(sp, b)])
                        ]),
                        b
                    )
                    // )
                ]))
                // )
            ])
            // }
        ));
    test_const = test_const.map(|mut tc| { tc.vis.node = ast::VisibilityKind::Public; tc});

    let test_extern = cx.item_extern_crate(sp, cx.ident_of("test"));

    // We use a whole module so that we can safely use the test crate
    let test_item = cx.item_mod(sp, sp,
        item.ident.clone().gensym(),
        vec![],
        vec![test_extern, test_const]);

    debug!("Synthetic test item:\n{}\n", pprust::item_to_string(&test_item));

    push(Annotatable::Item(test_item));
}

fn has_bench_signature(i: &ast::Item) -> bool {
    if let ast::ItemKind::Fn(ref decl, _, _, _) = i.node {
        // NB: inadequate check, but we're running
        // well before resolve, can't get too deep.
        decl.inputs.len() == 1
    } else {
        false
    }
}
