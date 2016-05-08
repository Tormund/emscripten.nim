import macros

when not defined(emscripten):
    {.error: "emscripten module may be imported only when compiling to emscripten target".}

type
    EMSCRIPTEN_WEBGL_CONTEXT_HANDLE* = cint
    EM_BOOL* = cint
    EMSCRIPTEN_RESULT* = cint

macro declTypeWithHeader(h: static[string]): stmt =
    result = parseStmt("type DummyHeaderType {.importc: \"void\", header:\"" & $h & "\".} = object")

template ensureHeaderIncluded(h: static[string]) =
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

template EM_ASM_INT*(code: static[string]): cint =
    block:
        ensureHeaderIncluded("<emscripten.h>")
        var emintres {.exportc.}: cint
        {.emit: "emintres = EM_ASM_INT_V({" & code & "});".}
        emintres

#TODO: Fix copy-paste
template EM_ASM_INT*(code: static[string], arg1: typed): cint =
    block:
        ensureHeaderIncluded("<emscripten.h>")
        var emintres {.exportc.}: cint
        let emintarg1 {.exportc.} = arg1
        {.emit: "emintres = EM_ASM_INT({" & code & "}, emintarg1);".}
        emintres

template EM_ASM_INT*(code: static[string], arg1: typed, arg2: typed): cint =
    block:
        ensureHeaderIncluded("<emscripten.h>")
        var emintres {.exportc.}: cint
        let emintarg1 {.exportc.} = arg1
        let emintarg2 {.exportc.} = arg2
        {.emit: "emintres = EM_ASM_INT({" & code & "}, emintarg1, emintarg2);".}
        emintres

template EM_ASM_INT*(code: static[string], arg1: typed, arg2: typed, arg3: typed): cint =
    block:
        ensureHeaderIncluded("<emscripten.h>")
        var emintres {.exportc.}: cint
        let emintarg1 {.exportc.} = arg1
        let emintarg2 {.exportc.} = arg2
        let emintarg3 {.exportc.} = arg3
        {.emit: "emintres = EM_ASM_INT({" & code & "}, emintarg1, emintarg2, emintarg3);".}
        emintres
