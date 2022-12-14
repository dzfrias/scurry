"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[5983],{3905:(e,t,r)=>{r.d(t,{Zo:()=>u,kt:()=>d});var n=r(7294);function o(e,t,r){return t in e?Object.defineProperty(e,t,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[t]=r,e}function a(e,t){var r=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),r.push.apply(r,n)}return r}function i(e){for(var t=1;t<arguments.length;t++){var r=null!=arguments[t]?arguments[t]:{};t%2?a(Object(r),!0).forEach((function(t){o(e,t,r[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(r)):a(Object(r)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(r,t))}))}return e}function l(e,t){if(null==e)return{};var r,n,o=function(e,t){if(null==e)return{};var r,n,o={},a=Object.keys(e);for(n=0;n<a.length;n++)r=a[n],t.indexOf(r)>=0||(o[r]=e[r]);return o}(e,t);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);for(n=0;n<a.length;n++)r=a[n],t.indexOf(r)>=0||Object.prototype.propertyIsEnumerable.call(e,r)&&(o[r]=e[r])}return o}var p=n.createContext({}),s=function(e){var t=n.useContext(p),r=t;return e&&(r="function"==typeof e?e(t):i(i({},t),e)),r},u=function(e){var t=s(e.components);return n.createElement(p.Provider,{value:t},e.children)},c={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},m=n.forwardRef((function(e,t){var r=e.components,o=e.mdxType,a=e.originalType,p=e.parentName,u=l(e,["components","mdxType","originalType","parentName"]),m=s(r),d=o,f=m["".concat(p,".").concat(d)]||m[d]||c[d]||a;return r?n.createElement(f,i(i({ref:t},u),{},{components:r})):n.createElement(f,i({ref:t},u))}));function d(e,t){var r=arguments,o=t&&t.mdxType;if("string"==typeof e||o){var a=r.length,i=new Array(a);i[0]=m;var l={};for(var p in t)hasOwnProperty.call(t,p)&&(l[p]=t[p]);l.originalType=e,l.mdxType="string"==typeof e?e:o,i[1]=l;for(var s=2;s<a;s++)i[s]=r[s];return n.createElement.apply(null,i)}return n.createElement.apply(null,r)}m.displayName="MDXCreateElement"},7022:(e,t,r)=>{r.r(t),r.d(t,{assets:()=>p,contentTitle:()=>i,default:()=>c,frontMatter:()=>a,metadata:()=>l,toc:()=>s});var n=r(7462),o=(r(7294),r(3905));const a={sidebar_position:3},i="Loops",l={unversionedId:"tutorial-basics/loops",id:"tutorial-basics/loops",title:"Loops",description:"Loops in Scurry are similar to those in modern languages.",source:"@site/docs/tutorial-basics/loops.md",sourceDirName:"tutorial-basics",slug:"/tutorial-basics/loops",permalink:"/docs/tutorial-basics/loops",draft:!1,tags:[],version:"current",sidebarPosition:3,frontMatter:{sidebar_position:3},sidebar:"tutorialSidebar",previous:{title:"If Statements",permalink:"/docs/tutorial-basics/if_stmt"},next:{title:"Switch Statements",permalink:"/docs/tutorial-basics/switch"}},p={},s=[{value:"While Loop",id:"while-loop",level:2},{value:"For Loop",id:"for-loop",level:2},{value:"Builtin Types",id:"builtin-types",level:3},{value:"Custom Iterators",id:"custom-iterators",level:3}],u={toc:s};function c(e){let{components:t,...r}=e;return(0,o.kt)("wrapper",(0,n.Z)({},u,r,{components:t,mdxType:"MDXLayout"}),(0,o.kt)("h1",{id:"loops"},"Loops"),(0,o.kt)("p",null,"Loops in Scurry are similar to those in modern languages."),(0,o.kt)("p",null,"There are two types of loops:"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("inlineCode",{parentName:"li"},"While")," loops"),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("inlineCode",{parentName:"li"},"For")," loops")),(0,o.kt)("h2",{id:"while-loop"},"While Loop"),(0,o.kt)("p",null,(0,o.kt)("inlineCode",{parentName:"p"},"While")," loops in Scurry have no surprises:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},'while 3 == 3 {\n   println("This will run forever!");\n}\n')),(0,o.kt)("p",null,(0,o.kt)("inlineCode",{parentName:"p"},"While")," loops follow the same boolean logic as ",(0,o.kt)("a",{parentName:"p",href:"./if_stmt"},"if statements"),"."),(0,o.kt)("h2",{id:"for-loop"},"For Loop"),(0,o.kt)("p",null,"Scurry uses the common ",(0,o.kt)("inlineCode",{parentName:"p"},"for ... in ...")," loop style, similar to\n",(0,o.kt)("a",{parentName:"p",href:"https://www.python.org"},"Python")," or ",(0,o.kt)("a",{parentName:"p",href:"https://www.rust-lang.org"},"Rust"),". Users\nfamiliar with those languages should pick up Scurry's ",(0,o.kt)("inlineCode",{parentName:"p"},"for")," loops in no time!"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},"for i in Range(0, 3) {\n   println(i);\n}\n")),(0,o.kt)("p",null,(0,o.kt)("inlineCode",{parentName:"p"},"Range")," is a builtin iterator, and iterators are what allows for the looping.\nIn this program, ",(0,o.kt)("inlineCode",{parentName:"p"},"0"),", ",(0,o.kt)("inlineCode",{parentName:"p"},"1"),", and ",(0,o.kt)("inlineCode",{parentName:"p"},"2")," will be printed."),(0,o.kt)("h3",{id:"builtin-types"},"Builtin Types"),(0,o.kt)("p",null,(0,o.kt)("a",{parentName:"p",href:"./types/array"},"Arrays")," can be looped through."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},"for item in [1, 2, 3] {\n   println(item);\n}\n")),(0,o.kt)("p",null,(0,o.kt)("a",{parentName:"p",href:"./types/string"},"Strings")," can be looped through."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},'for char in "Hello!" {\n   println(char);\n}\n')),(0,o.kt)("p",null,"Scurry does not have a dedicated ",(0,o.kt)("inlineCode",{parentName:"p"},"char")," type, so the ",(0,o.kt)("inlineCode",{parentName:"p"},"char")," variable in this\nexample will be another ",(0,o.kt)("a",{parentName:"p",href:"./types/string"},"String")),(0,o.kt)("p",null,(0,o.kt)("a",{parentName:"p",href:"./types/map"},"Maps")," can be looped through."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},'for key in {"test": 0, 10: 0} {\n   println(key);\n}\n')),(0,o.kt)("p",null,(0,o.kt)("inlineCode",{parentName:"p"},"key")," will be the keys of the map."),(0,o.kt)("h3",{id:"custom-iterators"},"Custom Iterators"),(0,o.kt)("p",null,"You can define your own iterator using the ",(0,o.kt)("inlineCode",{parentName:"p"},"Iterator")," component! More on that\nhere."))}c.isMDXComponent=!0}}]);