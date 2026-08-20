program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections, Generics.Defaults, Math;
const
	TT = 1000 * 1000;
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
	n, d, i, s, t, c: int32;
	ans: int64;
	sus: array [1 .. TT] of tlist<int32>;
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

	readln(n, d);

	for s := 1 to TT do sus[s] := tlist<int32>.create;
	pq := theap<int32>.Create(TComparer<Int32>.Construct(HeapCompare));

	for i := 1 to n do begin
		readln(s, t);
		sus[s].add(t);
	end;

	ans := 0;
	for s := 1 to TT do begin
		for t in sus[s] do pq.push(t);
		sus[s].free;

		if pq.count > 0 then begin
			t := pq.pop;
			while (t-s < d) and (pq.count > 0) do t := pq.pop;
			if t-s >= d then pq.push(t);
		end;

		c := pq.count;
		inc(ans, int64(c-1) * c div 2);
	end;

	writeln(ans);
	pq.free;
end.
