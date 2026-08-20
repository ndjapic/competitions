program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #graph #adjacency #list #heap
uses
	Generics.Collections, Generics.Defaults, SysUtils, Math;
const
	NN = 200 * 1000;
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
var
	n, m, i, j, a, b: int32;
	chi: array [1 .. NN] of TList<int32>;
	nop, ans: array [1 .. NN] of int32;
	pq: THeap<Int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function HeapCompare(constref Left, Right: Int32): int32;
begin
	Result := CompareValue(Left, Right);
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

	readln(n, m);

	for a := 1 to n do begin
		chi[a] := TList<int32>.Create;
		nop[a] := 0;
	end;

	for i := 1 to m do begin
		readln(a, b);
		chi[a].Add(b);
		inc(nop[b]);
	end;

	pq := THeap<Int32>.Create(TComparer<Int32>.Construct(HeapCompare));
	for a := 1 to n do
		if nop[a] = 0 then pq.Push(a);

	j := 0;
	while pq.Count > 0 do begin
		a := pq.Pop;
		inc(j);
		ans[j] := a;

		for b in chi[a] do begin
			dec(nop[b]);
			if nop[b] = 0 then pq.Push(b);
		end;
	end;

	if j < n then
		writeln(-1)
	else begin
		for j := 1 to n-1 do write(ans[j], ' ');
		writeln(ans[n]);
	end;

	pq.Free;
end.
