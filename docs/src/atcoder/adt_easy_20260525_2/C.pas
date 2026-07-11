program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #heap
uses
	Generics.Collections, Generics.Defaults, SysUtils, Math;
const
	NN = 100;
type
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
	TBox = record
		b, c: int8;
	end;
var
	n, q, i, b: int8;
	box: TBox;
	c: array [1 .. NN] of int8;
	pq: THeap<TBox>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function HeapCompare(constref l, r: TBox): int32;
begin
	Result := CompareValue(l.c, r.c);
	if Result = 0 then Result := CompareValue(l.b, r.b);
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

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);

	pq := THeap<TBox>.Create(TComparer<TBox>.Construct(HeapCompare));
	box.c := 0;
	for b := 1 to n do begin
		c[b] := 0;
		box.b := b;
		pq.Push(box);
	end;

	for i := 1 to q do begin
		read(b);
		box.b := b;

		if b = 0 then begin
			repeat
				box := pq.Pop;
				b := box.b;
			until c[b] = box.c;
		end;

		inc(c[b]);
		box.c := c[b];
		pq.Push(box);
		write(b);
		if i < q then write(' ');
	end;
	readln;
	writeln;

	pq.Free;
end.
