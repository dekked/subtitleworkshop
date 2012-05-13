unit VTInPlaceEdition;

// -----------------------------------------------------------------------------

interface

// -----------------------------------------------------------------------------

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Functions, 
  StdCtrls, VirtualTrees, ExtDlgs, ImgList, Buttons, ExtCtrls, ComCtrls,
  Mask, TreeViewHandle, USubtitlesFunctions, TimeMaskEdit, Undo;

// -----------------------------------------------------------------------------

type
  // Our own edit link to implement several different node editors.
  TTreeEditLink = class(TInterfacedObject, IVTEditLink)
  private
    FEdit: TWinControl;        // One of the property editor classes.
    FTree: TVirtualStringTree; // A back reference to the tree calling.
    FNode: PVirtualNode;       // The node being edited.
    FColumn: Integer;          // The column of the node being edited.
    FOldEditProc: TWndMethod;  // Used to capture some important messages
                               // regardless of the type of edit control we use
  protected
    procedure EditWindowProc(var Message: TMessage);
    procedure EditTimeChangeFromEditOnly(Sender: TObject; NewTime: Cardinal);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    destructor Destroy; override;

    function BeginEdit: Boolean; stdcall;
    function CancelEdit: Boolean; stdcall;
    function EndEdit: Boolean; stdcall;
    function GetBounds: TRect; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
    procedure ProcessMessage(var Message: TMessage); stdcall;
    procedure SetBounds(R: TRect); stdcall;
  end;

// -----------------------------------------------------------------------------

implementation

uses formMain;

//----------------- TTreeEditLink ----------------------------------------------

destructor TTreeEditLink.Destroy;
begin
  FEdit.Free;
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TTreeEditLink.EditWindowProc(var Message: TMessage);
// Here we can capture messages for keeping track of focus changes.
begin
  case Message.Msg of
    WM_KILLFOCUS: FTree.EndEditNode;
  else
    FOldEditProc(Message);
  end;
end;

// -----------------------------------------------------------------------------

procedure TTreeEditLink.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 case Key of
    VK_RETURN,
    VK_UP,
    VK_DOWN:
      begin
        //FEdit.Hide;
        FTree.EndEditNode;
        //FTree.RepaintNode(FNode);

        with FTree do
        begin
          if Assigned(FNode) then
          begin
            Selected[FNode] := False;
            if Key = VK_UP then
              FocusedNode := FNode.PrevSibling else
              FocusedNode := FNode.NextSibling;
            if Assigned(FocusedNode) then
            begin
              Selected[FocusedNode] := True;
              FTree.FocusedNode := FocusedNode;
            end;
            //if FTree.Enabled then SetFocus(FTree.Handle);
          end;
        end;     
        Key := 0;
      end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TTreeEditLink.EditTimeChangeFromEditOnly(Sender: TObject; NewTime: Cardinal);
var
  Data: PSubtitleItem;
begin
  Data := FTree.GetNodeData(FNode);
  case FColumn of
    1: Data.InitialTime := NewTime;
    2: Data.FinalTime := NewTime;
  end;
end;

// -----------------------------------------------------------------------------

function TTreeEditLink.BeginEdit: Boolean;
begin
  Result := True;
  FEdit.Show;
  FEdit.SetFocus;
  // Set a window procedure hook (aka subclassing) to get notified about important messages.
  FOldEditProc     := FEdit.WindowProc;
  FEdit.WindowProc := EditWindowProc;
end;

// -----------------------------------------------------------------------------

function TTreeEditLink.CancelEdit: Boolean;
begin
  Result := True;
  // Restore the edit's window proc.
  FEdit.WindowProc := FOldEditProc;
  FEdit.Hide;
end;

// -----------------------------------------------------------------------------

function TTreeEditLink.EndEdit: Boolean;
var
  Data: PSubtitleItem;
  S: WideString;
  P: TPoint;
  Dummy: Integer;
begin
  // Check if the place the user click on yields another node as the one we
  // are currently editing. If not then do not stop editing.
  GetCursorPos(P);
  P := FTree.ScreenToClient(P);
  Result := FTree.GetNodeAt(P.X, P.Y, True, Dummy) <> FNode;

  if Result then
  begin
    Data := FTree.GetNodeData(FNode);

    case FColumn of
      3:
        begin
          S := (FEdit as TEdit).Text;
          if S <> Data.Text then
          begin
            DetectChangesForUndo(Data.Text, ReplaceString(S, '|', #13#10), True);
            Data.Text := ReplaceString(S, '|', #13#10);
            FTree.InvalidateNode(FNode);
            frmMain.OrgModified := True;
            frmMain.lblText.Caption := Format(TextOrTransLength, [LabelText, GetLengthForEachLine(Data.Text)]);
          end;
        end;
      4:
        begin
          S := (FEdit as TEdit).Text;
          if S <> Data.Translation then
          begin
            DetectChangesForUndo(Data.Translation, ReplaceString(S, '|', #13#10), True);
            Data.Translation := ReplaceString(S, '|', #13#10);
            FTree.InvalidateNode(FNode);
            frmMain.TransModified := True;
            frmMain.lblTranslation.Caption := Format(TextOrTransLength, [LabelTranslation, GetLengthForEachLine(Data.Translation)]);
          end;
        end;
    end; 
    //FEdit.Hide;
    FTree.Refresh;
    FTree.RepaintNode(FNode);
  end;
end;

// -----------------------------------------------------------------------------

function TTreeEditLink.GetBounds: TRect;
begin
  Result := FEdit.BoundsRect;
end;

// -----------------------------------------------------------------------------

function TTreeEditLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean;
var
  Data: PSubtitleItem;
begin
  Result  := True;
  FTree   := Tree as TVirtualStringTree;
  FNode   := Node;
  FColumn := Column;

  // determine what edit type actually is needed
  FEdit.Free;
  FEdit := nil;
  Data := Tree.GetNodeData(FNode);

  case Column of
    1:
      begin
        FEdit := TTimeMaskEdit.Create(nil);
        with FEdit as TTimeMaskEdit do
        begin
          Visible     := False;
          Parent      := Tree;
          BorderStyle := bsNone;
          Time        := Data.InitialTime;
          OnTimeChangeFromEditOnly := EditTimeChangeFromEditOnly;
          SelStart := 10; // Milliseconds get focus by default
        end;
      end;
    2:
      begin
        FEdit := TTimeMaskEdit.Create(nil);
        with FEdit as TTimeMaskEdit do
        begin
          Visible     := False;
          Parent      := Tree;
          BorderStyle := bsNone;
          Time        := Data.FinalTime;
          OnTimeChangeFromEditOnly := EditTimeChangeFromEditOnly;
          SelStart := 10; // Milliseconds get focus by default
        end;
      end;
    3:
      begin
        FEdit := TEdit.Create(nil);
        with FEdit as TEdit do
        begin
          Visible      := False;
          Parent       := Tree;
          Ctl3D        := False;
          Text         := ReplaceString(Data.Text, #13#10, '|');
          OnKeyDown    := EditKeyDown;
          Font.Charset := frmMain.OrgCharset;
        end;
      end;
    4:
      begin
        FEdit := TEdit.Create(nil);
        with FEdit as TEdit do
        begin
          Visible      := False;
          Parent       := Tree;
          Ctl3D        := False;
          Text         := ReplaceString(Data.Translation, #13#10, '|');
          OnKeyDown    := EditKeyDown;
          Font.Charset := frmMain.TransCharset;
        end;
      end;
  else
    Result := False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TTreeEditLink.ProcessMessage(var Message: TMessage);
begin
  FEdit.WindowProc(Message);
end;

// -----------------------------------------------------------------------------

procedure TTreeEditLink.SetBounds(R: TRect);
var
  Dummy: Integer;
begin
  // Since we don't want to activate grid extensions in the tree (this would influence how the selection is drawn)
  // we have to set the edit's width explicitly to the width of the column.
  FTree.Header.Columns.GetColumnBounds(FColumn, Dummy, R.Right);
  FEdit.BoundsRect := R;
end;

// -----------------------------------------------------------------------------

end.
