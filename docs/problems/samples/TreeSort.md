# Problem: TreeSort.pas

```pascal
program TreeSortGenerics;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults;
type
	TItem = record
		x, y: Integer;
		constructor Create(ax, ay: Integer);
	end;

constructor TItem.Create(ax, ay: Integer);
begin
	x := ax; y := ay;
end;

{ Генеричка TreeSort процедура }
procedure GenericTreeSort<T>(var arr: array of T; comparer: IComparer<T>);
type
	PNode = ^TNode;
	TNode = record
		key: T;
		l, r: PNode;
	end;
var
	root: PNode;
	idx, r_limit, temp_idx: Integer;

	procedure Insert(var node: PNode; const val: T);
	begin
		if node = nil then begin
			new(node);
			node^.key := val;
			node^.l := nil; node^.r := nil;
		end else if comparer.Compare(val, node^.key) < 0 then 
			Insert(node^.l, val)
		else 
			Insert(node^.r, val);
	end;

	procedure FillAndFree(node: PNode);
	begin
		if node <> nil then begin
			FillAndFree(node^.l);
			arr[idx] := node^.key;
			inc(idx);
			FillAndFree(node^.r);
			dispose(node);
		end;
	end;

begin
	root := nil; Randomize;
	for r_limit := High(arr) downto 0 do begin
		temp_idx := Random(r_limit + 1);
		Insert(root, arr[temp_idx]);
		arr[temp_idx] := arr[r_limit]; 
	end;
	idx := 0;
	FillAndFree(root);
end;

{ Конкретна функција поређења за наш рекорд }
function ItemCompare(constref a, b: TItem): Integer;
begin
	Result := a.x - b.x;
	if Result = 0 then Result := a.y - b.y;
end;

var
	data: array of TItem;
	i: Integer;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	SetLength(data, 5);
	data[0] := TItem.Create(10, 5); data[1] := TItem.Create(2, 20);
	data[2] := TItem.Create(10, 2); data[3] := TItem.Create(5, 5);
	data[4] := TItem.Create(2, 10);

	GenericTreeSort<TItem>(data, TComparer<TItem>.Construct(ItemCompare));

	for i := 0 to High(data) do 
		WriteLn('(', data[i].x, ', ', data[i].y, ')');
end.

```
