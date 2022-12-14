"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[10],{3905:(e,t,n)=>{n.d(t,{Zo:()=>u,kt:()=>m});var r=n(7294);function i(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function a(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function l(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?a(Object(n),!0).forEach((function(t){i(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):a(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function s(e,t){if(null==e)return{};var n,r,i=function(e,t){if(null==e)return{};var n,r,i={},a=Object.keys(e);for(r=0;r<a.length;r++)n=a[r],t.indexOf(n)>=0||(i[n]=e[n]);return i}(e,t);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);for(r=0;r<a.length;r++)n=a[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(i[n]=e[n])}return i}var p=r.createContext({}),o=function(e){var t=r.useContext(p),n=t;return e&&(n="function"==typeof e?e(t):l(l({},t),e)),n},u=function(e){var t=o(e.components);return r.createElement(p.Provider,{value:t},e.children)},d={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},c=r.forwardRef((function(e,t){var n=e.components,i=e.mdxType,a=e.originalType,p=e.parentName,u=s(e,["components","mdxType","originalType","parentName"]),c=o(n),m=i,h=c["".concat(p,".").concat(m)]||c[m]||d[m]||a;return n?r.createElement(h,l(l({ref:t},u),{},{components:n})):r.createElement(h,l({ref:t},u))}));function m(e,t){var n=arguments,i=t&&t.mdxType;if("string"==typeof e||i){var a=n.length,l=new Array(a);l[0]=c;var s={};for(var p in t)hasOwnProperty.call(t,p)&&(s[p]=t[p]);s.originalType=e,s.mdxType="string"==typeof e?e:i,l[1]=s;for(var o=2;o<a;o++)l[o]=n[o];return r.createElement.apply(null,l)}return r.createElement.apply(null,n)}c.displayName="MDXCreateElement"},7406:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>p,contentTitle:()=>l,default:()=>d,frontMatter:()=>a,metadata:()=>s,toc:()=>o});var r=n(7462),i=(n(7294),n(3905));const a={sidebar_position:4},l="String",s={unversionedId:"tutorial-basics/types/string",id:"tutorial-basics/types/string",title:"String",description:"The builtin String type in Scurry.",source:"@site/docs/tutorial-basics/types/string.md",sourceDirName:"tutorial-basics/types",slug:"/tutorial-basics/types/string",permalink:"/docs/tutorial-basics/types/string",draft:!1,tags:[],version:"current",sidebarPosition:4,frontMatter:{sidebar_position:4},sidebar:"tutorialSidebar",previous:{title:"Bool",permalink:"/docs/tutorial-basics/types/bool"},next:{title:"Array",permalink:"/docs/tutorial-basics/types/array"}},p={},o=[{value:"Behavior",id:"behavior",level:2},{value:"Methods",id:"methods",level:2},{value:"len()",id:"len",level:3},{value:"trim()",id:"trim",level:3},{value:"starts_with(s: String)",id:"starts_withs-string",level:3},{value:"ends_with(s: String)",id:"ends_withs-string",level:3},{value:"substring(n1: Int, n2: Int)",id:"substringn1-int-n2-int",level:3},{value:"split(delim: String)",id:"splitdelim-string",level:3}],u={toc:o};function d(e){let{components:t,...n}=e;return(0,i.kt)("wrapper",(0,r.Z)({},u,n,{components:t,mdxType:"MDXLayout"}),(0,i.kt)("h1",{id:"string"},"String"),(0,i.kt)("p",null,"The builtin ",(0,i.kt)("inlineCode",{parentName:"p"},"String")," type in Scurry."),(0,i.kt)("h2",{id:"behavior"},"Behavior"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},"Immutable/passed by value"),(0,i.kt)("li",{parentName:"ul"},"Can be concatenated with the ",(0,i.kt)("inlineCode",{parentName:"li"},"+")," operator",(0,i.kt)("pre",{parentName:"li"},(0,i.kt)("code",{parentName:"pre"},'>> "Hello" + " world!";\n"Hello world!"\n'))),(0,i.kt)("li",{parentName:"ul"},"Can be indexed",(0,i.kt)("pre",{parentName:"li"},(0,i.kt)("code",{parentName:"pre"},'>> "Test"[1];\n"e"\n'))),(0,i.kt)("li",{parentName:"ul"},"Falsey when empty"),(0,i.kt)("li",{parentName:"ul"},"Supports unicode characters",(0,i.kt)("pre",{parentName:"li"},(0,i.kt)("code",{parentName:"pre"},'>> "\u4e2d\u6587"[0];\n"\u4e2d"\n'))),(0,i.kt)("li",{parentName:"ul"},"Can be looped through",(0,i.kt)("pre",{parentName:"li"},(0,i.kt)("code",{parentName:"pre"},' >> for char in "String" { println(char); }\n S\n t\n r\n i\n n\n g\n'))),(0,i.kt)("li",{parentName:"ul"},"Quotes can be escaped with a ",(0,i.kt)("inlineCode",{parentName:"li"},"\\"),(0,i.kt)("pre",{parentName:"li"},(0,i.kt)("code",{parentName:"pre"},'>> "this is valid: \\"!!";\n"this is valid: \\"!!"\n')))),(0,i.kt)("h2",{id:"methods"},"Methods"),(0,i.kt)("p",null,"The ",(0,i.kt)("inlineCode",{parentName:"p"},"String")," type has the following methods."),(0,i.kt)("h3",{id:"len"},"len()"),(0,i.kt)("p",null,"Returns an ",(0,i.kt)("a",{parentName:"p",href:"/docs/tutorial-basics/types/int"},"Int")," containing the number of characters."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre"},'>> "test".len();\n4\n')),(0,i.kt)("h3",{id:"trim"},"trim()"),(0,i.kt)("p",null,"Returns a new ",(0,i.kt)("inlineCode",{parentName:"p"},"String")," with all whitespace on the left and right ends stripped\naway."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre"},'>> " hello ".trim();\n"hello"\n')),(0,i.kt)("h3",{id:"starts_withs-string"},"starts_with(s: String)"),(0,i.kt)("p",null,"Returns ",(0,i.kt)("inlineCode",{parentName:"p"},"True")," if the string starts with ",(0,i.kt)("inlineCode",{parentName:"p"},"s"),", but ",(0,i.kt)("inlineCode",{parentName:"p"},"False")," otherwise."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre"},'>> "does it start with?".starts_with("does");\nTrue\n')),(0,i.kt)("h3",{id:"ends_withs-string"},"ends_with(s: String)"),(0,i.kt)("p",null,"Returns ",(0,i.kt)("inlineCode",{parentName:"p"},"True")," if the string ends with ",(0,i.kt)("inlineCode",{parentName:"p"},"s"),", but ",(0,i.kt)("inlineCode",{parentName:"p"},"False")," otherwise."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre"},'>> "doesn\'t end with".starts_with("what");\nFalse\n')),(0,i.kt)("h3",{id:"substringn1-int-n2-int"},"substring(n1: Int, n2: Int)"),(0,i.kt)("p",null,"Returns a new ",(0,i.kt)("inlineCode",{parentName:"p"},"String")," that is within ",(0,i.kt)("inlineCode",{parentName:"p"},"n1")," and ",(0,i.kt)("inlineCode",{parentName:"p"},"n2"),". ",(0,i.kt)("inlineCode",{parentName:"p"},"n1")," is inclusive, ",(0,i.kt)("inlineCode",{parentName:"p"},"n2")," is\nnot."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre"},'>> "get a substring!".substring(0, 4);\n"get"\n')),(0,i.kt)("h3",{id:"splitdelim-string"},"split(delim: String)"),(0,i.kt)("p",null,"Return an ",(0,i.kt)("a",{parentName:"p",href:"array"},"Array")," consisting of the string split by ",(0,i.kt)("inlineCode",{parentName:"p"},"delim"),"."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre"},'>> "split into three".split(" ");\n["split", "into", "three"]\n')))}d.isMDXComponent=!0}}]);