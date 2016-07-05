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

type EmscriptenUiEvent* = object
    detail*: clong
    documentBodyClientWidth*: cint
    documentBodyClientHeight*: cint
    windowInnerWidth*: cint
    windowInnerHeight*: cint
    windowOuterWidth*: cint
    windowOuterHeight*: cint
    scrollTop*: cint
    scrollLeft*: cint

type EmscriptenWheelEvent* = object
    mouse*: EmscriptenMouseEvent
    deltaX*: cdouble
    deltaY*: cdouble
    deltaZ*: cdouble
    deltaMode*: culong

type EmscriptenKeyboardEvent* = object
    key*: array[32, char]
    code*: array[32, char]
    location*: culong
    ctrlKey*: EM_BOOL
    shiftKey*: EM_BOOL
    altKey*: EM_BOOL
    metaKey*: EM_BOOL
    repeat*: EM_BOOL
    locale*: array[32, char]
    charValue*: array[32, char]
    charCode*: culong
    keyCode*: culong
    which*: culong

type EmscriptenFocusEvent* = object
    nodeName*: array[128, char]
    id*: array[128, char]

type em_callback_func* = proc() {.cdecl.}
type em_arg_callback_func* = proc(p: pointer) {.cdecl.}
type em_str_callback_func* = proc(s: cstring) {.cdecl.}
type em_async_wget_onload_func* = proc(a: pointer, p: pointer, sz: cint) {.cdecl.}
type em_mouse_callback_func* = proc(eventType: cint, mouseEvent: ptr EmscriptenMouseEvent, userData: pointer): EM_BOOL {.cdecl.}
type em_ui_callback_func* = proc (eventType: cint, uiEvent: ptr EmscriptenUiEvent, userData: pointer): EM_BOOL {.cdecl.}
type em_wheel_callback_func* = proc(eventType: cint, wheelEvent: ptr EmscriptenWheelEvent, userData: pointer): EM_BOOL {.cdecl.}
type em_key_callback_func* = proc(eventType: cint, keyEvent: ptr EmscriptenKeyboardEvent, userData: pointer): EM_BOOL {.cdecl.}
type em_focus_callback_func* = proc(eventType: cint, focusEvet: ptr EmscriptenFocusEvent, userData: pointer): EM_BOOL {.cdecl.}

{.push importc.}
proc emscripten_webgl_init_context_attributes*(attributes: ptr EmscriptenWebGLContextAttributes)
proc emscripten_webgl_create_context*(target: cstring, attributes: ptr EmscriptenWebGLContextAttributes): EMSCRIPTEN_WEBGL_CONTEXT_HANDLE
proc emscripten_webgl_make_context_current*(context: EMSCRIPTEN_WEBGL_CONTEXT_HANDLE): EMSCRIPTEN_RESULT
proc emscripten_set_main_loop*(f: em_callback_func, fps, simulate_infinite_loop: cint)
proc emscripten_cancel_main_loop*()

proc emscripten_set_mousedown_callback*(target: cstring, userData: pointer, useCapture: EM_BOOL, callback: em_mouse_callback_func): EMSCRIPTEN_RESULT
proc emscripten_set_mouseup_callback*(target: cstring, userData: pointer, useCapture: EM_BOOL, callback: em_mouse_callback_func): EMSCRIPTEN_RESULT
proc emscripten_set_mousemove_callback*(target: cstring, userData: pointer, useCapture: EM_BOOL, callback: em_mouse_callback_func): EMSCRIPTEN_RESULT

proc emscripten_set_wheel_callback*(target: cstring, userData: pointer, useCapture: EM_BOOL, callback: em_wheel_callback_func): EMSCRIPTEN_RESULT

proc emscripten_set_resize_callback*(target: cstring, userData: pointer, useCapture: EM_BOOL, callback: em_ui_callback_func): EMSCRIPTEN_RESULT
proc emscripten_set_scroll_callback*(target: cstring, userData: pointer, useCapture: EM_BOOL, callback: em_ui_callback_func): EMSCRIPTEN_RESULT

proc emscripten_get_mouse_status*(mouseState: ptr EmscriptenMouseEvent): EMSCRIPTEN_RESULT

proc emscripten_async_wget_data*(url: cstring, arg: pointer, onload: em_async_wget_onload_func, onerror: em_arg_callback_func)

proc emscripten_set_keypress_callback*(target: cstring, userData: pointer, useCapture: EM_BOOL, callback: em_key_callback_func): EMSCRIPTEN_RESULT
proc emscripten_set_keydown_callback*(target: cstring, userData: pointer, useCapture: EM_BOOL, callback: em_key_callback_func): EMSCRIPTEN_RESULT
proc emscripten_set_keyup_callback*(target: cstring, userData: pointer, useCapture: EM_BOOL, callback: em_key_callback_func): EMSCRIPTEN_RESULT

proc emscripten_set_blur_callback*(target: cstring, userData: pointer, useCapture: EM_BOOL, callback: em_focus_callback_func): EMSCRIPTEN_RESULT
proc emscripten_set_focus_callback*(target: cstring, userData: pointer, useCapture: EM_BOOL, callback: em_focus_callback_func): EMSCRIPTEN_RESULT
{.pop.}

proc addPragma*(someProc, pragma: NimNode) =
    ## Adds pragma to callable definition
    var pragmaNode = someProc.pragma
    if pragmaNode.isNil or pragmaNode.kind == nnkEmpty:
        pragmaNode = newNimNode(nnkPragma)
        someProc.pragma = pragmaNode
    pragmaNode.add(pragma)

macro EMSCRIPTEN_KEEPALIVE*(someProc: typed): typed =
    result = someProc
    #[
      Ident !"exportc"
      ExprColonExpr
        Ident !"codegenDecl"
        StrLit __attribute__((used)) $# $#$#
    ]#
    result.addPragma(newIdentNode("exportc"))
    result.addPragma(newNimNode(nnkExprColonExpr).add(
            newIdentNode("codegenDecl"),
            newLit("__attribute__((used)) $# $#$#")))

template EM_ASM*(code: static[string]) =
    ensureHeaderIncluded("<emscripten.h>")
    {.emit: "EM_ASM(" & code & ");".}

proc emAsmAux*(code: string, args: NimNode, resTypeName, emResType: string): NimNode =
    #echo "CODE: ", code

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
                newIdentNode(resTypeName),
                newEmptyNode()
            )
        )
    )

    var emitStr = ""
    if args.len == 0:
        emitStr = "emintres = EM_ASM_" & emResType & "_V({" & code & "});"
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
        emitStr = "emintres = EM_ASM_" & emResType & "({" & code & "}"
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

macro EM_ASM_INT*(code: static[string], args: varargs[typed]): cint =
    result = emAsmAux(code, args, "cint", "INT")

macro EM_ASM_DOUBLE*(code: static[string], args: varargs[typed]): cdouble =
    result = emAsmAux(code, args, "cdouble", "DOUBLE")

proc emscripten_async_wget_data*(url: cstring, onload: proc(data: pointer, sz: cint), onerror: proc()) =
    ## Helper wrapper for emscripten_async_wget_data to pass nim closures around
    type Ctx = ref object
        onload: proc(data: pointer, sz: cint)
        onerror: proc()

    var ctx: Ctx
    ctx.new()
    ctx.onload = onload
    ctx.onerror = onerror
    GC_ref(ctx)

    proc onLoadWrapper(arg: pointer, data: pointer, sz: cint) {.cdecl.} =
        let c = cast[Ctx](arg)
        GC_unref(c)
        c.onload(data, sz)

    proc onErrorWrapper(arg: pointer) {.cdecl.} =
        let c = cast[Ctx](arg)
        GC_unref(c)
        c.onerror()

    emscripten_async_wget_data(url, cast[pointer](ctx), onLoadWrapper, onErrorWrapper)

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