program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections, Generics.Defaults, SysUtils, Math;

type
	THorse = record
		i: int8;
		t: int32;
	end;
	THeap<T> = class
	private
		FList: TList<T>;
		FComparer: IComparer<T>;
		function GetFavoriteChild(ParentIdx: int32): int32;
	public
		constructor Create(AComparer: IComparer<T>);
		destructor Destroy; override;
		procedure Push(const Item: T);
		function Pop: T;
		function Count: int32;
	end;

function HeapCompare(constref Left, Right: THorse): int32;
begin
	Result := CompareValue(Left.t, Right.t);
end;

constructor THeap<T>.Create(AComparer: IComparer<T>);
begin
	FList := TList<T>.Create;
	FComparer := AComparer;
end;

destructor THeap<T>.Destroy;
begin
	FList.Free;
	inherited;
end;

function THeap<T>.Count: int32;
begin
	Result := FList.Count;
end;

function THeap<T>.GetFavoriteChild(ParentIdx: int32): int32;
var
	L, R: int32;
begin
	L := ParentIdx * 2 + 1;
	R := L + 1;
	Result := L;
	if (R < FList.Count) and (FComparer.Compare(FList[R], FList[L]) < 0) then
		Result := R;
end;

procedure THeap<T>.Push(const Item: T);
var
	Idx, ParentIdx: int32;
begin
	Idx := FList.Add(Item);
	ParentIdx := (Idx - 1) div 2;
	while (Idx > 0) and (FComparer.Compare(FList[Idx], FList[ParentIdx]) < 0) do begin
		FList.Exchange(Idx, ParentIdx);
		Idx := ParentIdx;
		ParentIdx := (Idx - 1) div 2;
	end;
end;

function THeap<T>.Pop: T;
var
	Idx, ChildIdx: int32;
begin
	Result := FList[0];
	FList[0] := FList[FList.Count - 1];
	FList.Delete(FList.Count - 1);
	
	Idx := 0;
	ChildIdx := GetFavoriteChild(Idx);
	while (ChildIdx < FList.Count) and (FComparer.Compare(FList[ChildIdx], FList[Idx]) < 0) do begin
		FList.Exchange(Idx, ChildIdx);
		Idx := ChildIdx;
		ChildIdx := GetFavoriteChild(Idx);
	end;
end;

var
	n, i: int8;
	elm: THorse;
	horses: THeap<THorse>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n);
	horses := THeap<THorse>.Create(TComparer<THorse>.Construct(HeapCompare));

	for i := 1 to n do begin
		elm.i := i;
		read(elm.t);
		horses.Push(elm);
	end;
	readln;

	writeln(horses.Pop.i, ' ', horses.Pop.i, ' ', horses.Pop.i);
	horses.Free;
end.
