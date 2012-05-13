{Need forms that keep proportionality when being resized manually?
And also when being Maximized or Tiled/Cascaded?
Forms for both MDI and SDI type applications?
TNFormSizing is all you need for that.
Just put the TNFormSizing on your form and choose an option
for ProportionType - its only published property. Job's done!

NOTICE: Please see the the explanatory article for details on this
component's advanced features, and for examples of usage.}

unit NFormSizing;

interface

{$WARN SYMBOL_DEPRECATED OFF}

uses
  Windows, Messages, Classes, Forms;

var
  {Messages to use during  your custom TileWindows() and CascadeWindows() calls.
  See the explanatory article text for details and example}
  FS_TILE_CASCADE_START,           // send before the call
  FS_TILE_CASCADE_END  : Cardinal; // send after the call

{type TProportionType  = (ptNone ,ptClientRect,ptWindowRect); }

type
  TNFormSizing = class(TComponent)
  private
    {*********         Properties fields          *********}
    FLetResizeWhenTileCascade: boolean;
    {*********         Other fields               *********}
    {Form subclassing info}
    FFormWndProc: LongInt;      //Form's inherent window procedure
    FFormHandle: HWND;          // Form's handle
    FmyParent: TForm;           // Form itself (as TForm)
    FNewFormProcedure: pointer; // Our own hook to Form's procedure
    {MDI Client window subclassing info (used in case of MDI child forms)}
    FMDIClientWndProc: LongInt;
    FMDIClientHandle: HWND;
    FNewMDIClientProcedure: pointer;
    {Reference dimensions that determine Form proportion ratios}
    Fw: Word;   // 1)  width /
    Fh: Word;   //            height ratio.
    Fcw: Word;  // 2)  clientwidth /
    Fch: Word;  //                   clientheight ratio.
    {Some relevant system parameters, that we get via GetSystemMetrics() calls}
    F_SM_CXMIN: Word; // minimum Width of a window you can manually resize to
    F_SM_CYMIN: Word; // minimum Height of a window you can manually resize to
    F_SM_CXSIZEFRAME: Word; // width of the horizontal sizing border
    F_SM_CYSIZEFRAME: Word; // height of the vertical sizing border
    F_SM_CXMAXTRACK: Word;  // maximum Width of a window you can manually resize to
    F_SM_CYMAXTRACK: Word;  // maximum Height of a window you can manually resize to
    {Some form state indicators}
    FMDIChild: boolean; //is this an MDI application
    FManualSizing: boolean; // is the form being sized manually (pulling the corner/border)
    FMaxMin: boolean; // is form (being) maximized or minimized;
    FMDIMaximize: boolean;    // is MDI form (being) maximized
    FMDITileCascade: boolean; // is current resize due to MDI Tile/Cascade
    FCustomTileCascade: boolean; // is TileWindows() or CascadeWindows() called
    {Some other variables}
    FDesktopWidth: Word;  //* free (from Taskbar) Desktop (in SDI applications),
    FDesktopHeight: Word; //* or MDI Client window dimensions (in MDI apps).
    FSetRatiosNotUsed: boolean; // wether SetClientRatio() or SetRatio() methods were at least once used
    procedure NewFormWndProc(var Mess: TMessage); // to subclass the Form, and
    procedure NewMDIClientWndProc(var Mess: TMessage); // MDI Client window
    procedure HookToParents; //* subclass Form, and if MDI - also Client window
    procedure UnhookFromParents; //* reverse operation
    function myGetDesktopRect: TRect; // get free desktop size, or MDI client size
    {individual message processing routines used in NewFormWndProc}
    function WMWINDOWPOSCHANGING(var Mess: TMessage): boolean;
    procedure WMGETMINMAXINFO(var Mess: TMessage);
    procedure WMSIZE(var Mess: TMessage);
    procedure WMACTIVATE(var Mess: TMessage);
    {standard IDE - generated property Set procedures}
    procedure SetLetResizeWhenTileCascade(const Value: boolean);
  protected
    { Protected declarations}
  public
    procedure SetRatio(RW, RH: Word);
    procedure SetClientRatio(RCW, RCH: Word);
    property LetResizeWhenTileCascade: boolean read FLetResizeWhenTileCascade write SetLetResizeWhenTileCascade;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReadSystemMetrics; // set F_SM_C... fields - window metrics
  published
  end;

procedure Register;

implementation

const
  TaskbarWindowClass = 'Shell_TrayWnd';
  QuitMessageString = 'My MDI form is being closed';

var
  QuitMessage: Cardinal; //* to send when the MDI child form is closed,
                         //* is registered in the "initialization" section of this unit

procedure Register;
begin
  RegisterComponents('Samples', [TNFormSizing]);
end;

{ TNafSizingConstraints }

{*************************************************************************}
constructor TNFormSizing.Create(AOwner: TComponent);
var
  i,j: integer;
  IsMaximized: integer;
begin
  inherited create(AOwner);
  if AOwner is TForm then // check if the user hasn't put us on the DataModule instead
  begin
    FmyParent := TForm(AOwner);
    FFormHandle := FmyParent.handle;
    {let the user know  if any other TNFomSizing instances exist on this form}
    if csDesigning in ComponentState then
    begin
      j := -1;
      for i := 0 to  FmyParent.ComponentCount-1 do
        if  FmyParent.Components[i].ClassName = ClassName then
          inc(j);
      if j > 0 then
        {j contains here the number of extra TNFomSizing components}
        messagebox(FFormHandle, pchar('Hint - there is at least one TNFomSizing component already on this Form.'+#13#13+' (No need to have more than one, actually.)'),
        'TNFormSizing create message',
        MB_OK or MB_ICONINFORMATION);
    end
    {and if not in design mode then subclass (hook to) the Parent Form}
    else begin
      LetResizeWhenTileCascade := true; // be it the default value
      ReadSystemMetrics;
      SetRatio(FmyParent.Width, FmyParent.Height); // set some initial non-zero values
      SetClientRatio(FmyParent.ClientWidth, FmyParent.ClientHeight); // set some initial non-zero values
      FSetRatiosNotUsed := true; // just reset it
      HookToParents;
      {When a new MDI Child window is being opened as MAXIMIZED, sometimes it's not positioned
      correctly - maybe smth. to do with the VCL. Stepping down the Z-order and then back helps:}
      if FMDIChild then
        //if FmyParent.WindowState = wsMaximized then
        {unfortunately, for initially maximized windows, state is not yet reflected
        here by the VCL. So we determine that ourselves:}
        SendMessage(FMDIClientHandle, WM_MDIGETACTIVE, 0, integer(@IsMaximized));
        if IsMaximized <> 0 then
        begin
          PostMessage(FMDIClientHandle, WM_MDINEXT, 0, 1); // previous
          PostMessage(FMDIClientHandle, WM_MDINEXT, 0, 0); // back
        end;
    end;
  end;
end;
{*************************************************************************}
destructor TNFormSizing.Destroy;
begin
  UnhookFromParents;  //for SDIs actually.(MDIs are unhooked on WM_MDIDESTROY earlier)
  inherited Destroy;
end;
{*************************************************************************}
{returns Free (from Taskbar) Desktop rectangle for SDI forms, and
MDI Client window rectangle for MDI child forms}
function TNFormSizing.myGetDesktopRect: TRect;
var
  TaskbarRect,
  ScreenRect: TRect;
  TaskbarHandle: HWND; // buffer, so as not to call  "FindWindow()" twice here
begin
  if FMDIChild then
  begin
    getwindowrect(FMDIClientHandle,result);
  end
  else begin
    TaskbarHandle := findwindow(TaskbarWindowClass,nil);
    if TaskbarHandle = 0 then
      setrect(TaskbarRect,0,0,0,0)
    else
      getwindowrect(TaskbarHandle,TaskbarRect);

    getwindowrect(GetDesktopWindow,ScreenRect);
    subtractrect(Result,ScreenRect,TaskbarRect);
  end;
end;
{*************************************************************************}
{a  "hook" to the MDI Client window procedure}
procedure TNFormSizing.NewMDIClientWndProc(var Mess: TMessage);
begin
  case Mess.Msg of

  WM_MDICASCADE,WM_MDITILE:
    FMDITileCascade := true; 

  WM_MDIDESTROY:
    if Mess.WParam = integer(FFormHandle) then // if it's us being destroyed
      UnhookFromParents;// this happens earlier than Destroy method, and it works safer here for MDIs

  else if Mess.Msg = QuitMessage then  // if some of colleagues is being destroyed
    begin
      {move the hook closer if it's our hook proc. mentioned in the message}
      if Mess.WParam = FMDIClientWndProc then
      begin
        FMDIClientWndProc := Mess.LParam;
        exit; // no need to pass further this message intended for this component instance
      end;
    end;
  end; // end message case

  {check if the programmer informs us about TileWindows() or CascadeWindows call}
  if Mess.Msg = FS_TILE_CASCADE_START then
    FCustomTileCascade := true;
  if Mess.Msg = FS_TILE_CASCADE_END then
    FCustomTileCascade := false;

  {pass message to the original window procedure}
  Mess.Result := CallWindowProc(pointer(FMDIClientWndProc),FMDIClientHandle,Mess.Msg,Mess.WParam,Mess.LParam);;
end;
{*************************************************************************}
procedure TNFormSizing.WMACTIVATE(var Mess: TMessage);
var
  myrect: TRect;
begin
  {no need to do anything if the Form is being de-activated}
  if (Mess.Msg = WM_ACTIVATE) and (LOWORD(Mess.WParam) = WA_INACTIVE) then exit;
  {Refresh each time the form is activated in case user re-sized the taskbar in the meantime.
  Doing that HERE is frequent enough to keep well informed about the Desktop size,
  and not too frequent to overload  message processing}
  myrect := myGetDesktopRect;
  FDesktopWidth := myrect.Right-myrect.Left;
  FDesktopHeight :=  myrect.Bottom-myrect.Top;
end;
{*************************************************************************}
procedure TNFormSizing.WMGETMINMAXINFO(var Mess: TMessage);
begin
  {let the system know your tracking requirements for non-proportional resizes}
  PMinMaxInfo(Mess.lparam)^.ptMinTrackSize.x := FmyParent.Constraints.Minwidth;
  PMinMaxInfo(Mess.lparam)^.ptMinTrackSize.y := FmyParent.Constraints.Minheight;
  {max tracking constraints are required to be non-zero by API}
  if  FmyParent.Constraints.Maxwidth > 0 then
    PMinMaxInfo(Mess.lparam)^.ptMaxTrackSize.x := FmyParent.Constraints.Maxwidth;
  if FmyParent.Constraints.Maxheight > 0 then
    PMinMaxInfo(Mess.lparam)^.ptMaxTrackSize.y := FmyParent.Constraints.Maxheight;
end;
{*************************************************************************}
{the sequence of commands in this procedure IS REALLY important}
procedure TNFormSizing.WMSIZE(var Mess: TMessage);
begin
  if FMDIChild then
  begin
    if Mess.WParam = SIZE_MAXIMIZED then
    begin
      FMDIMaximize := true;
      {helps to wipe away from the screen any "trash" which may sometimes occur when
      switching between maximized MDI child forms, though slows down re-paint a little.
      Remove this line if you wish.}
      redrawwindow(getparent(FmyParent.handle),nil,0,RDW_ERASE or RDW_INVALIDATE);
    end
    else
      FMDIMaximize := false;
  end;

  if Mess.WParam <> SIZE_RESTORED then
    FMaxMin := true;

  {If programmatical change then update reference dimensions, but not otherwise.}
  {So, ONLY if current resize was initiated by user programmatically
  (like "PictureForm1.Width := 222;")  - and that excludes:
  1) manual sizing, 2) Maximizing or Minimizing, 3) or manual Tile/Cascade (by right
  clicking on the Taskbar and selecting from context menu), 4) MDI Tiling,
  5) the programmer informs us about his own TileWindows() or CascadeWindows() call}


  {These two IFs below should be in that same order}
  {When MDI child is maximized and user presses Tile/Cascade, then instead of one
  WM_SIZE form gets TWO of them (both with SIZE_RESTORED). This IF takes care of the extra one}
  if not FMaxMin then
    FMDITileCascade := false;

  {When maximizing/minimizing each SIZE_MAXIMIZED/SIZE_MINIMIZED WM_SIZE mesage is
  always accompanied by one SIZE_RESTORED. We don't let that second Message change the ratios by
  putting the FMaxMin state indicator reset here at the end }
  if Mess.WParam = SIZE_RESTORED then
    FMaxMin := false;

  {here is a good place to put your test messages:}
  //FmyParent.Caption := format('FcwxFch=%dx%d',[Fcw,Fch]);
  //FmyParent.Caption := format('FwxFc=%dx%d',[Fw,Fh]);
end;
{*************************************************************************}
function TNFormSizing.WMWINDOWPOSCHANGING(var Mess: TMessage): boolean;
begin
  result := false;
  if (PWindowPos(Mess.LParam)^.cx = 0) or (PWindowPos(Mess.LParam)^.cy = 0) then
    exit  // if Z-order change, then pass further to the old window procedure
  else
    result := true; //don't pass futher any sizing operations, we do them here
end;
{*************************************************************************}
{it's a regular "hook" to the parent form procedure.}
procedure TNFormSizing.NewFormWndProc(var Mess: TMessage);
begin
  case Mess.Msg of
    WM_SIZING:
    begin
      Mess.Result := integer(true); // or any non-zero value
      exit; // don't pass this message further
    end;

    WM_SIZE:
        WMSIZE(Mess);

    {if Windows asks you the sizing rectangle min/max dimensions}
    WM_GETMINMAXINFO:
    begin
      WMGETMINMAXINFO(Mess);
      Mess.result := 0;
      exit;  // no need to pass this message further
    end;

    {sent once BEFORE the manual resize begins}
    WM_ENTERSIZEMOVE:
      FManualSizing := true;

    {sent once AFTER the manual resize is finished}
    WM_EXITSIZEMOVE:
      FManualSizing := false;

    WM_ACTIVATE, WM_CHILDACTIVATE:
      WMACTIVATE(Mess);

    WM_WINDOWPOSCHANGING:  // process Cascade/Tile resizes
      if WMWINDOWPOSCHANGING(Mess) then
      begin
        Mess.Result := 0;
        exit; // don't pass this message further
      end;

    WM_SETTINGCHANGE:      //* sent when window metrics have been changed, like
      ReadSystemMetrics; //* for example by the user in the "Display Properties"

  end; // end message CASE

  {check if the programmer informs us about TileWindows() or CascadeWindows call}
  if Mess.Msg = FS_TILE_CASCADE_START then
    FCustomTileCascade := true;
  if Mess.Msg = FS_TILE_CASCADE_END then
    FCustomTileCascade := false;

  {pass the message to the form's old window procedure}
  Mess.Result := CallWindowProc(pointer(FFormWndProc),FFormHandle,Mess.Msg,Mess.WParam,Mess.LParam);;
end;
{*************************************************************************}
procedure TNFormSizing. SetRatio(RW, RH: Word);
begin
  if (RW > 0) and (RH > 0) then
  begin
    Fw := RW;
    Fh := RH;
    FSetRatiosNotUsed := false;
  end;
end;
{*************************************************************************}
procedure TNFormSizing.SetClientRatio(RCW, RCH: Word);
begin
  if (RCW > 0) and  (RCH > 0) then
  begin
    Fcw := RCW;
    Fch := RCH;
    FSetRatiosNotUsed :=  false;
  end;
end;
{*************************************************************************}
procedure TNFormSizing.SetLetResizeWhenTileCascade(const Value: boolean);
begin
  FLetResizeWhenTileCascade := Value;
end;
{*************************************************************************}
{WM_SETTINGCHANGE – the message that Win32 uses to notify all top level windows
about changes in window metrics – for example when the user changes border size 
in the "Display Properties" - IS NOT RECEIVED by MDI Child forms nor by MDI 
Client window.
MDI Parent just re-draws its children according to the new metrics,
but doesn't re-direct this message to them.
 You can make MDI Parent form can catch WM_SETTINGCHANGE, and call TNFormSizing.ReadSystemMetrics
method of its children concerned to update metrics info of their TNFormSizing components.
I've decided to leave it at that, instead of additionally complicating the
component by subclassing MDI Parent form.
- In fact ignoring WM_SETTINGCHANGE message doesn't lead to any disasters, and in most cases
your users just wouldn't notice its effects - unless they set window border size to some
incredibly large value.}
procedure TNFormSizing.ReadSystemMetrics;
begin
  F_SM_CXMIN := GetSystemMetrics(SM_CXMIN);
  F_SM_CYMIN := GetSystemMetrics(SM_CYMIN);
  F_SM_CXSIZEFRAME := GetSystemMetrics(SM_CXSIZEFRAME);
  F_SM_CYSIZEFRAME := GetSystemMetrics(SM_CYSIZEFRAME);
  F_SM_CXMAXTRACK := GetSystemMetrics(SM_CXMAXTRACK);
  F_SM_CYMAXTRACK := GetSystemMetrics(SM_CYMAXTRACK);
end;
{*************************************************************************}
procedure TNFormSizing.HookToParents;
begin
  FNewFormProcedure := MakeObjectInstance(NewFormWndProc);
  {subclass and keep the old procedure pointer}
  FFormWndProc := SetWindowLong(FFormHandle,GWL_WNDPROC,integer(FNewFormProcedure));
  {if MDI app, then subclass the MDI Client window - to get MDI Tile/Cascade Messages}
  FMDIChild :=  FmyParent.FormStyle = fsMDIChild;
  if FMDIChild then
  begin
    FMDIClientHandle := GetParent(FFormHandle);
    FNewMDIClientProcedure := makeobjectinstance(NewMDIClientWndProc);
    FMDIClientWndProc := setwindowlong(FMDIClientHandle,GWL_WNDPROC,integer(FNewMDIClientProcedure));
  end;
end;
{*************************************************************************}
procedure TNFormSizing.UnhookFromParents;
begin
  {Unhook Form window procedure}
  if FNewFormProcedure <> nil then
  begin
    SetWindowLong(FFormHandle,GWL_WNDPROC,FFormWndProc);
    FreeObjectInstance(FNewFormProcedure);
    FNewFormProcedure := nil;
  end;
  {Unhook MDI Client window procedure}
  if FNewMDIClientProcedure <> nil then
  begin
    {notify all the colleagues that this hook is being released form the hooks chain}
    SendMessage(FMDIClientHandle, QuitMessage, integer(FNewMDIClientProcedure), integer(FMDIClientWndProc));
    {if our hook to MDI Client was the last one}
    if GetWindowLong(FMDIClientHandle,GWL_WNDPROC) =  integer(FNewMDIClientProcedure) then
      SetWindowLong(FMDIClientHandle,GWL_WNDPROC,FMDIClientWndProc); // unhook
    FreeObjectInstance(FNewMDIClientProcedure);
    FNewMDIClientProcedure := nil;
  end;
end;

initialization
  QuitMessage := RegisterWindowMessage(QuitMessageString);

  {for the cases when Tile/Cascade functions are called explicitly}
  FS_TILE_CASCADE_START := RegisterWindowMessage('Before TileWindow() or CascadeWindow()');
  FS_TILE_CASCADE_END := RegisterWindowMessage('After TileWindow() or CascadeWindow()');
end.
