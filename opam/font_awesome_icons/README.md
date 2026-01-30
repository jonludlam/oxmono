Font-Awesome Icons
==================

This is a library containing a set of font awesome icons for use in Bonsai apps. The iconset is fetched
from jane/external/font-awesome-icons which itself is pulled from <https://github.com/FortAwesome/Font-Awesome>

Below is example usage of using an svg icon from the font_awesome_icons library within a Bonsai project. 

jbuild
```
(rule (
  (targets (font_awesome_icons.ml font_awesome_icons.mli))
  (deps ())
  (action
   "%{bin:embed-font-awesome-icons} -output font_awesome_icons brands-amazon-pay")))

(enforce_style (
  (exceptions (font_awesome_icons.ml font_awesome_icons.mli)) (let_syntax ())))

```

ml
```
open Bonsai_web.Cont 

Vdom.Node.inner_html_svg 
~tag:"svg"
~attrs:[ Vdom.Attr.empty ]
~this_html_is_sanitized_and_is_totally_safe_trust_me:Font_awsome_icons.brands_amazon_pay_dot_svg
```
 
