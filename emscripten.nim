import macros

when not defined(emscripten):
    {.error: "emscripten module may be imported only when compiling to emscripten target".}

type
    EMSCRIPTEN_WEBGL_CONTEXT_HANDLE* = cint
    EM_BOOL* = cint
    EMSCRIPTEN_RESULT* = cint

macro declTypeWithHeader(h: static[string]): stmt =
    result = parseStmt("type DummyHeaderType {.importc: \"void\", header:\"" & $h & "\".} = object")

template ensureHeaderIncluded(h: static[string]): stmt =
    block:
        declTypeWithHeader(h)
        var tht : ptr DummyHeaderType = nil

type EmscriptenWebGLContextAttributes* = object
    alpha*: EM_BOOL
    depth*: EM_BOOL
    stencil*: EM_BOOL
    antialias*: EM_BOOL
    premultipliedAlpha*: EM_BOOL
    preserveDrawingBuffer*: EM_BOOL
    preferLowPowerToHighPerformance*: EM_BOOL
    failIfMajorPerformanceCaveat*: EM_BOOL

    majorVersion*: cint
    minorVersion*: cint

    enableExtensionsByDefault*: EM_BOOL

type EmscriptenMouseEvent* = object
    timestamp*: cdouble
    screenX*: clong
    screenY*: clong
    clientX*: clong
    clientY*: clong
    ctrlKey*: EM_BOOL
    shiftKey*: EM_BOOL
    altKey*: EM_BOOL
    metaKey*: EM_BOOL
    button*: uint16
    buttons*: uint16
    movementX*: clong
    movementY*: clong
    targetX*: clong
    targetY*: clong
    canvasX*: clong
    canvasY*: clong
    padding*: clong

type em_callback_func* = proc() {.cdecl.}
type em_mouse_callback_func* = proc(eventType: cint, mouseEvent: ptr EmscriptenMouseEvent, userData: pointer): EM_BOOL {.cdecl.}

{.push importc.}
proc emscripten_webgl_init_context_attributes*(attributes: ptr EmscriptenWebGLContextAttributes)
proc emscripten_webgl_create_context*(target: cstring, attributes: ptr EmscriptenWebGLContextAttributes): EMSCRIPTEN_WEBGL_CONTEXT_HANDLE
proc emscripten_webgl_make_context_current*(context: EMSCRIPTEN_WEBGL_CONTEXT_HANDLE): EMSCRIPTEN_RESULT
proc emscripten_set_main_loop*(f: em_callback_func, fps, simulate_infinite_loop: cint)
proc emscripten_cancel_main_loop*()

proc emscripten_set_mousedown_callback*(target: cstring, userData: pointer, useCapture: EM_BOOL, callback: em_mouse_callback_func): EMSCRIPTEN_RESULT
proc emscripten_set_mouseup_callback*(target: cstring, userData: pointer, useCapture: EM_BOOL, callback: em_mouse_callback_func): EMSCRIPTEN_RESULT
{.pop.}

macro EMSCRIPTEN_KEEPALIVE*(someProc: typed): typed =
    result = someProc
    #[
      Ident !"exportc"
      ExprColonExpr
        Ident !"codegenDecl"
        StrLit __attribute__((used)) $# $#$#
    ]#
    result.pragma = newNimNode(nnkPragma).add(
        newIdentNode("exportc"),
        newNimNode(nnkExprColonExpr).add(
            newIdentNode("codegenDecl"),
            newLit("__attribute__((used)) $# $#$#")))

template EM_ASM*(code: static[string]) =
    ensureHeaderIncluded("<emscripten.h>")
    {.emit: "EM_ASM(" & code & ");".}

macro EM_ASM_INT*(code: static[string], args: varargs[typed]): cint =
    result = newNimNode(nnkStmtList)
    result.add(newCall(bindSym "ensureHeaderIncluded", newLit("<emscripten.h>")))

    result.add(
        newNimNode(nnkVarSection).add(
            newNimNode(nnkIdentDefs).add(
                newNimNode(nnkPragmaExpr).add(
                    newIdentNode("emintres"),
                    newNimNode(nnkPragma).add(
                        newIdentNode("exportc")
                    )
                ),
                newIdentNode("cint"),
                newEmptyNode()
            )
        )
    )

    var emitStr = ""
    if args.len == 0:
        emitStr = "emintres = EM_ASM_INT_V({" & code & "});"
    else:
        let argsSection = newNimNode(nnkLetSection)
        for i in 0 ..< args.len:
            argsSection.add(
                newNimNode(nnkIdentDefs).add(
                    newNimNode(nnkPragmaExpr).add(
                        newIdentNode("emintarg" & $i),
                        newNimNode(nnkPragma).add(
                            newIdentNode("exportc")
                        )
                    ),
                    newEmptyNode(),
                    args[i]
                )
            )
        result.add(argsSection)
        emitStr = "emintres = EM_ASM_INT({" & code & "}"
        for i in 0 ..< args.len:
            emitStr &= ", emintarg" & $i
        emitStr &= ");"

    result.add(
        newNimNode(nnkPragma).add(
            newNimNode(nnkExprColonExpr).add(
                newIdentNode("emit"),
                newLit(emitStr)
            )
        )
    )

    result.add(newIdentNode("emintres"))

    result = newNimNode(nnkBlockStmt).add(
        newEmptyNode(),
        result
    )

#[
dumpTree:
    block:
        ensureHeaderIncluded("<emscripten.h>")
        var emintres {.exportc.}: cint
        var arg1 = 0
        var arg2 = 0
        let
            emintarg1 {.exportc.} = arg1
            emintarg2 {.exportc.} = arg2

        {.emit: "emintres = EM_ASM_INT({" & "asdf" & "}, emintarg1);".}
        emintres
]#