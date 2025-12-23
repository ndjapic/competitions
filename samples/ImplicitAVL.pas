{$MODE DELPHI}
{ Implicit AVL array with segment-aggregate support
	- Generic implementation (use "specialize")
	- O(log n) insert/delete by index
	- Bisection helpers (lower_bound / upper_bound) that can be used to insert into a sorted sequence
		NOTE: bisection uses binary search over indices and therefore costs O(log^2 n) (each access by index is O(log n)).
	- Segment-tree-like range query (associative merge provided by user) and point update

	Usage example (at bottom):
		type TArr = specialize TImplicitAVL<Integer>;
		var A: TArr;
		A := TArr.Create(@IntCompare, @IntMerge, 0);
		A.InsertAt(0, 10);
		writeln(A.RangeQuery(0, A.Count-1));
		A.Free;
}

unit ImplicitAVL;

interface

uses SysUtils;

type
	generic TImplicitAVL<T> = class
	public
		type
			TCompare = function(const a,b: T): Integer; // return <0 if a<b, 0 if =, >0 if a>b
			TMerge = function(const a,b: T): T; // associative merge for segment queries

	strict private
		type
			PNode = ^TNode;
			TNode = record
				value: T;
				left, right: PNode;
				height: Integer;
				cnt: Integer; // size of subtree
				agg: T; // aggregated value for subtree
			end;

	private
		FRoot: PNode;
		FCompare: TCompare;
		FMerge: TMerge;
		FNeutral: T; // neutral element for merge

		function NewNode(const v: T): PNode;
		procedure DisposeNode(p: PNode);
		function Height(p: PNode): Integer; inline;
		function CountOf(p: PNode): Integer; inline;
		procedure UpdateNode(p: PNode);
		function BalanceFactor(p: PNode): Integer; inline;
		function RotateRight(y: PNode): PNode;
		function RotateLeft(x: PNode): PNode;
		function Balance(p: PNode): PNode;

		// index-based helpers (0-based)
		function GetAtNode(p: PNode; idx: Integer): T;
		function SetAtNode(p: PNode; idx: Integer; const v: T): PNode;
		function InsertAtNode(p: PNode; idx: Integer; const v: T): PNode;
		function DeleteAtNode(p: PNode; idx: Integer): PNode;
		function JoinLeftmost(p: PNode; const v: T): PNode;
		function JoinRightmost(p: PNode; const v: T): PNode;

		// lower/upper bound by value using binary search on indices (cost O(log^2 n))
		function GetIndexValue(idx: Integer): T;
		function LowerBoundBS(const v: T): Integer;
		function UpperBoundBS(const v: T): Integer;

		// range query helper
		function RangeQueryNode(p: PNode; l, r: Integer): T;

		procedure FreeNodeRecursive(p: PNode);
	public
		constructor Create(CompareFunc: TCompare; MergeFunc: TMerge; const Neutral: T);
		destructor Destroy; override;

		// basic properties
		function Count: Integer; inline;
		function IsEmpty: Boolean; inline;

		// index-based operations
		procedure InsertAt(idx: Integer; const v: T); // 0..Count
		procedure DeleteAt(idx: Integer); // 0..Count-1
		function GetAt(idx: Integer): T;
		procedure SetAt(idx: Integer; const v: T);

		// bisection for sorted sequences
		function LowerBound(const v: T): Integer; // first pos where a[pos] >= v (0..Count)
		function UpperBound(const v: T): Integer; // first pos where a[pos] > v
		procedure InsertSorted(const v: T); // insert keeping sorted order (using LowerBound)

		// segment-tree-like functionalities
		function RangeQuery(l, r: Integer): T; // inclusive [l,r]

		// utility
		procedure Clear;
	end;

implementation

{ Implementation }

function GenericZero<T>: T;
begin
	// returns zero-initialized value (works for basic types/records)
	FillChar(Result, SizeOf(Result), 0);
end;

{ TImplicitAVL<T> }

constructor TImplicitAVL.Create(CompareFunc: TCompare; MergeFunc: TMerge; const Neutral: T);
begin
	FRoot := nil;
	FCompare := CompareFunc;
	FMerge := MergeFunc;
	FNeutral := Neutral;
end;

destructor TImplicitAVL.Destroy;
begin
	Clear;
	inherited;
end;

function TImplicitAVL.Count: Integer;
begin
	Result := CountOf(FRoot);
end;

function TImplicitAVL.IsEmpty: Boolean;
begin
	Result := FRoot = nil;
end;

function TImplicitAVL.NewNode(const v: T): PNode;
begin
	New(Result);
	Result^.value := v;
	Result^.left := nil;
	Result^.right := nil;
	Result^.height := 1;
	Result^.cnt := 1;
	Result^.agg := v;
end;

procedure TImplicitAVL.DisposeNode(p: PNode);
begin
	Dispose(p);
end;

function TImplicitAVL.Height(p: PNode): Integer;
begin
	if p = nil then Exit(0);
	Result := p^.height;
end;

function TImplicitAVL.CountOf(p: PNode): Integer;
begin
	if p = nil then Exit(0);
	Result := p^.cnt;
end;

procedure TImplicitAVL.UpdateNode(p: PNode);
begin
	if p = nil then Exit;
	p^.height := 1 + Max(Height(p^.left), Height(p^.right));
	p^.cnt := 1 + CountOf(p^.left) + CountOf(p^.right);
	// recompute aggregate
	p^.agg := p^.value;
	if p^.left <> nil then p^.agg := FMerge(p^.left^.agg, p^.agg);
	if p^.right <> nil then p^.agg := FMerge(p^.agg, p^.right^.agg);
end;

function TImplicitAVL.BalanceFactor(p: PNode): Integer;
begin
	if p = nil then Exit(0);
	Result := Height(p^.left) - Height(p^.right);
end;

function TImplicitAVL.RotateRight(y: PNode): PNode;
var x, T2: PNode;
begin
	x := y^.left;
	T2 := x^.right;
	// rotate
	x^.right := y;
	y^.left := T2;
	// update
	UpdateNode(y);
	UpdateNode(x);
	Result := x;
end;

function TImplicitAVL.RotateLeft(x: PNode): PNode;
var y, T2: PNode;
begin
	y := x^.right;
	T2 := y^.left;
	y^.left := x;
	x^.right := T2;
	UpdateNode(x);
	UpdateNode(y);
	Result := y;
end;

function TImplicitAVL.Balance(p: PNode): PNode;
begin
	if p = nil then Exit(nil);
	UpdateNode(p);
	if BalanceFactor(p) > 1 then
	begin
		if BalanceFactor(p^.left) < 0 then
			p^.left := RotateLeft(p^.left);
		Exit(RotateRight(p));
	end;
	if BalanceFactor(p) < -1 then
	begin
		if BalanceFactor(p^.right) > 0 then
			p^.right := RotateRight(p^.right);
		Exit(RotateLeft(p));
	end;
	Result := p;
end;

function TImplicitAVL.GetAtNode(p: PNode; idx: Integer): T;
var leftCnt: Integer;
begin
	if p = nil then raise Exception.Create('Index out of bounds');
	leftCnt := CountOf(p^.left);
	if idx < leftCnt then Exit(GetAtNode(p^.left, idx));
	if idx = leftCnt then Exit(p^.value);
	Exit(GetAtNode(p^.right, idx - leftCnt - 1));
end;

function TImplicitAVL.SetAtNode(p: PNode; idx: Integer; const v: T): PNode;
var leftCnt: Integer;
begin
	if p = nil then raise Exception.Create('Index out of bounds');
	leftCnt := CountOf(p^.left);
	if idx < leftCnt then p^.left := SetAtNode(p^.left, idx, v)
	else if idx = leftCnt then p^.value := v
	else p^.right := SetAtNode(p^.right, idx - leftCnt - 1, v);
	UpdateNode(p);
	Result := Balance(p);
end;

function TImplicitAVL.JoinLeftmost(p: PNode; const v: T): PNode;
begin
	if p = nil then Exit(NewNode(v));
	p^.left := JoinLeftmost(p^.left, v);
	UpdateNode(p);
	Result := Balance(p);
end;

function TImplicitAVL.JoinRightmost(p: PNode; const v: T): PNode;
begin
	if p = nil then Exit(NewNode(v));
	p^.right := JoinRightmost(p^.right, v);
	UpdateNode(p);
	Result := Balance(p);
end;

function TImplicitAVL.InsertAtNode(p: PNode; idx: Integer; const v: T): PNode;
var leftCnt: Integer;
begin
	if p = nil then
	begin
		Result := NewNode(v);
		Exit;
	end;
	leftCnt := CountOf(p^.left);
	if idx <= leftCnt then p^.left := InsertAtNode(p^.left, idx, v)
	else p^.right := InsertAtNode(p^.right, idx - leftCnt - 1, v);
	UpdateNode(p);
	Result := Balance(p);
end;

function TImplicitAVL.DeleteAtNode(p: PNode; idx: Integer): PNode;
var leftCnt: Integer;
		tmp: PNode;
begin
	if p = nil then raise Exception.Create('Index out of bounds');
	leftCnt := CountOf(p^.left);
	if idx < leftCnt then p^.left := DeleteAtNode(p^.left, idx)
	else if idx > leftCnt then p^.right := DeleteAtNode(p^.right, idx - leftCnt - 1)
	else
	begin
		// delete this node
		if (p^.left = nil) or (p^.right = nil) then
		begin
			tmp := p^.left;
			if tmp = nil then tmp := p^.right;
			DisposeNode(p);
			Result := tmp;
			Exit;
		end
		else
		begin
			// find inorder successor (leftmost of right)
			tmp := p^.right;
			while tmp^.left <> nil do tmp := tmp^.left;
			p^.value := tmp^.value;
			p^.right := DeleteAtNode(p^.right, 0); // delete leftmost in right subtree
		end;
	end;
	if Result = nil then
	begin
		// continue balancing from p
	end;
	if p <> nil then
	begin
		UpdateNode(p);
		Result := Balance(p);
	end else Result := nil;
end;

procedure TImplicitAVL.InsertAt(idx: Integer; const v: T);
begin
	if (idx < 0) or (idx > Count) then raise Exception.Create('InsertAt: index out of range');
	FRoot := InsertAtNode(FRoot, idx, v);
end;

procedure TImplicitAVL.DeleteAt(idx: Integer);
begin
	if (idx < 0) or (idx >= Count) then raise Exception.Create('DeleteAt: index out of range');
	FRoot := DeleteAtNode(FRoot, idx);
end;

function TImplicitAVL.GetAt(idx: Integer): T;
begin
	if (idx < 0) or (idx >= Count) then raise Exception.Create('GetAt: index out of range');
	Result := GetAtNode(FRoot, idx);
end;

procedure TImplicitAVL.SetAt(idx: Integer; const v: T);
begin
	if (idx < 0) or (idx >= Count) then raise Exception.Create('SetAt: index out of range');
	FRoot := SetAtNode(FRoot, idx, v);
end;

function TImplicitAVL.GetIndexValue(idx: Integer): T;
begin
	Result := GetAt(idx);
end;

function TImplicitAVL.LowerBoundBS(const v: T): Integer;
var lo, hi, mid: Integer;
		cur: T;
begin
	lo := 0; hi := Count; // search in [0..Count]
	while lo < hi do
	begin
		mid := (lo + hi) div 2;
		cur := GetIndexValue(mid);
		if FCompare(cur, v) < 0 then lo := mid + 1
		else hi := mid;
	end;
	Result := lo;
end;

function TImplicitAVL.UpperBoundBS(const v: T): Integer;
var lo, hi, mid: Integer;
		cur: T;
begin
	lo := 0; hi := Count;
	while lo < hi do
	begin
		mid := (lo + hi) div 2;
		cur := GetIndexValue(mid);
		if FCompare(cur, v) <= 0 then lo := mid + 1
		else hi := mid;
	end;
	Result := lo;
end;

function TImplicitAVL.LowerBound(const v: T): Integer;
begin
	// wrapper - O(log^2 n)
	Result := LowerBoundBS(v);
end;

function TImplicitAVL.UpperBound(const v: T): Integer;
begin
	Result := UpperBoundBS(v);
end;

procedure TImplicitAVL.InsertSorted(const v: T);
begin
	InsertAt(LowerBound(v), v);
end;

function TImplicitAVL.RangeQueryNode(p: PNode; l, r: Integer): T;
var leftCnt, curIdx: Integer;
		resLeft, resRight, resMid: T;
begin
	if (p = nil) or (l > r) then Exit(FNeutral);
	// current node index
	leftCnt := CountOf(p^.left);
	curIdx := leftCnt; // index of p in its subtree
	if (l = 0) and (r = CountOf(p)-1) then
	begin
		Result := p^.agg; Exit; // whole subtree
	end;
	// query left part
	if l < curIdx then
		resLeft := RangeQueryNode(p^.left, l, Min(r, curIdx-1))
	else resLeft := FNeutral;
	// current node
	if (l <= curIdx) and (curIdx <= r) then resMid := p^.value else resMid := FNeutral;
	// query right part
	if r > curIdx then
		resRight := RangeQueryNode(p^.right, Max(0, l - curIdx - 1), r - curIdx - 1)
	else resRight := FNeutral;
	// merge results in order: left, mid, right
	Result := resLeft;
	if not (CompareMem(@resMid, @FNeutral, SizeOf(FNeutral))) then
		Result := FMerge(Result, resMid)
	;
	if not (CompareMem(@resRight, @FNeutral, SizeOf(FNeutral))) then
		Result := FMerge(Result, resRight);
end;

function TImplicitAVL.RangeQuery(l, r: Integer): T;
begin
	if (l < 0) or (r >= Count) or (l > r) then raise Exception.Create('RangeQuery: invalid range');
	Result := RangeQueryNode(FRoot, l, r);
end;

procedure TImplicitAVL.FreeNodeRecursive(p: PNode);
begin
	if p = nil then Exit;
	FreeNodeRecursive(p^.left);
	FreeNodeRecursive(p^.right);
	DisposeNode(p);
end;

procedure TImplicitAVL.Clear;
begin
	FreeNodeRecursive(FRoot);
	FRoot := nil;
end;

end.

{ Example usage (put in your program's uses clause and instantiate with 'specialize') }

{ Example:
program TestImplicitAVL;
uses SysUtils, ImplicitAVL;

function IntCompare(const a,b: Integer): Integer;
begin
	Result := a - b;
end;

function IntMerge(const a,b: Integer): Integer;
begin
	Result := a + b; // sum as aggregate
end;

var arr: specialize TImplicitAVL<Integer>;
begin
	arr := specialize TImplicitAVL<Integer>.Create(@IntCompare, @IntMerge, 0);
	arr.InsertAt(0, 5);
	arr.InsertAt(1, 3);
	arr.InsertAt(1, 4); // [5,4,3]
	Writeln(arr.GetAt(0)); // 5
	Writeln(arr.GetAt(1)); // 4
	Writeln(arr.RangeQuery(0,2)); // sum = 12
	arr.InsertSorted(4); // will insert at pos where >=4 (O(log^2 n))
	arr.Free;
end.
}
