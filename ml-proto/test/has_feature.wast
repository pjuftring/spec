(module
  (func $has_feature
    (has_feature "simd128"))
  (export "has_feature" $has_feature)
)

(assert_return (invoke "has_feature") (i32.const 0))
