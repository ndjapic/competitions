program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #custom #sort #heap
uses
	Generics.Defaults, Generics.Collections, Math;
const
	NN = 200 * 1000;
type
	TPrinter = record
		w, t: int32;
	end;
	THeap<_T> = class
	private
		FItems: TList<_T>;
		FComparer: IComparer<_T>;
		function Favorite(u: SizeInt): SizeInt;
		function GetItem(Index: SizeInt): _T;
		function GetCount: SizeInt;
	public
		constructor Create(AComparer: IComparer<_T>);
		destructor Destroy; override;
		procedure Push(Item: _T);
		function Pop: _T;
		property Count: SizeInt read GetCount;
		property Items[Index: SizeInt]: _T read GetItem; default;
	end;
var
	n, m, i, j, k: int32;
	ans: int64;
	printers: TList<TPrinter>;
	jobs: TList<int32>;
	pq: THeap<int32>;
	t: array [0 .. NN] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function ComparePrinters(constref l, r: TPrinter): Integer;
begin
	Result := CompareValue(l.w, r.w);
	if Result = 0 then
		Result := CompareValue(r.t, l.t);
end;

function ReadPrinters(n: int32): TList<TPrinter>;
var
	i: int32;
	x: TPrinter;
begin
	Result := TList<TPrinter>.Create;
	Result.Capacity := n;
	for i := 0 to n-1 do begin
		ReadLn(x.w, x.t);
		Result.Add(x);
		Result.Exchange(i, Random(i+1));
	end;
	Result.Sort(TComparer<TPrinter>.Construct(ComparePrinters));
end;

function ReadJobs(n: int32): TList<int32>;
var
	i, x: int32;
begin
	Result := TList<int32>.Create;
	Result.Capacity := n;
	for i := 0 to n-1 do begin
		ReadLn(x);
		Result.Add(x);
		Result.Exchange(i, Random(i+1));
	end;
	Result.Sort;
end;

// HeapCompare
function HeapCompare(constref Lhs, Rhs: Int32): int32;
begin
	Result := CompareValue(t[Lhs], t[Rhs]);
end;

// THeap<_T>
function THeap<_T>.Favorite(u: SizeInt): SizeInt;
var
	v: SizeInt;
begin
	v := u * 2 + 2;
	if (v >= Count) or (FComparer.Compare(FItems[v-1], FItems[v]) < 0) then
		dec(v);
	Result := v;
end;

function THeap<_T>.GetItem(Index: SizeInt): _T;
begin
	Result := FItems[Index];
end;

function THeap<_T>.GetCount: SizeInt;
begin
	Result := FItems.Count;
end;

constructor THeap<_T>.Create(AComparer: IComparer<_T>);
begin
	Inherited Create;
	FItems := TList<_T>.Create;
	FComparer := AComparer;
	{FItems.OwnsObjects := False;}
end;

destructor THeap<_T>.Destroy;
begin
	FItems.Free;
	inherited Destroy;
end;

procedure THeap<_T>.Push(Item: _T);
var
	u, v: SizeInt;
begin
	v := Count;
	FItems.Add(Item);
	u := (v-1) div 2;
	while (v > 0) and (FComparer.Compare(FItems[v], FItems[u]) < 0) do begin
		FItems.Exchange(u, v);
		v := u;
		u := (v-1) div 2;
	end;
end;

function THeap<_T>.Pop: _T;
var
	u, v: SizeInt;
begin
	Result := FItems[0];
	FItems[0] := FItems[Count - 1];
	FItems.Delete(Count - 1);

	u := 0;
	v := Favorite(u);
	while (v < Count) and (FComparer.Compare(FItems[v], FItems[u]) < 0) do begin
		FItems.Exchange(u, v);
		u := v;
		v := Favorite(u);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(n, m);
	printers := ReadPrinters(n);
	jobs := ReadJobs(m);
	pq := THeap<int32>.Create(TComparer<int32>.Construct(HeapCompare));

	i := n-1;
	ans := 0;

	for j := m-1 downto 0 do
		if ans > -1 then begin
			while (i >= 0) and (printers[i].w >= jobs[j]) do begin
				t[i] := printers[i].t;
				pq.Push(i);
				dec(i);
			end;

			if pq.Count = 0 then
				ans := -1
			else begin
				k := pq.Pop;
				ans := max(ans, t[k]);
				inc(t[k], printers[k].t);
				pq.Push(k);
			end;
		end;

	writeln(ans);

	printers.Free;
	jobs.Free;
	pq.Free;
end.
