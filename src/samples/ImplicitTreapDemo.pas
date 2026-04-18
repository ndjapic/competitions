program ImplicitTreapDemo;

uses
	Math, SysUtils; // For Randomize, Random, and memory management (Dispose/New)

type
	PNode = ^TNode;
	TNode = record
		priority: Integer;
		size: Integer;
		value: Integer; // The actual data stored in the array
		left, right: PNode;
	end;

var
	Root: PNode;

// Helper function to get the size of a subtree
function GetSize(T: PNode): Integer;
begin
	if T = nil then
		GetSize := 0
	else
		GetSize := T^.size;
end;

// Helper function to update the size of a node after child changes
procedure UpdateSize(var T: PNode);
begin
	if T <> nil then
		T^.size := GetSize(T^.left) + GetSize(T^.right) + 1;
end;

// Creates a new treap node
function CreateNode(val: Integer): PNode;
var
	newNode: PNode;
begin
	New(newNode);
	newNode^.value := val;
	newNode^.priority := Random(MaxInt); // Assign a random priority
	newNode^.size := 1;
	newNode^.left := nil;
	newNode^.right := nil;
	CreateNode := newNode;
end;

// Merges two treaps L and R into one, assuming all elements in L appear before
// all elements in R in the final ordered sequence.
function Merge(L, R: PNode): PNode;
begin
	if L = nil then
		Merge := R
	else if R = nil then
		Merge := L
	else if L^.priority > R^.priority then
	begin
		// L becomes the root, merge L^.right and R
		L^.right := Merge(L^.right, R);
		UpdateSize(L);
		Merge := L;
	end
	else
	begin
		// R becomes the root, merge L and R^.left
		R^.left := Merge(L, R^.left);
		UpdateSize(R);
		Merge := R;
	end;
end;

// Splits a treap T into two treaps, L and R, where L contains the first K elements
// (0 to K-1 positions) and R contains the remaining elements.
procedure Split(T: PNode; var L, R: PNode; K: Integer);
var
	CurrentKey: Integer;
begin
	if T = nil then
	begin
		L := nil;
		R := nil;
		Exit;
	end;

	CurrentKey := GetSize(T^.left) + 1; // Implicit key (1-based index)

	if CurrentKey <= K then
	begin
		// T and its left subtree go to L. Recurse on T^.right.
		Split(T^.right, T^.right, K - CurrentKey);
		L := T;
		// R is set by the recursive call's R parameter
	end
	else
	begin
		// T and its right subtree go to R. Recurse on T^.left.
		Split(T^.left, T^.left, K);
		R := T;
		// L is set by the recursive call's L parameter
	end;

	UpdateSize(T);
end;

// Inserts a value at a specific position (0-based pos)
procedure Insert(var Root: PNode; pos, val: Integer);
var
	L, R, NewNodeP: PNode;
begin
	Split(Root, L, R, pos);
	NewNodeP := CreateNode(val);
	Root := Merge(Merge(L, NewNodeP), R);
end;

// Deletes the element at a specific position (0-based pos)
procedure Delete(var Root: PNode; pos: Integer);
var
	L, Mid, R: PNode;
begin
	Split(Root, L, Mid, pos); // L has first `pos` elements
	Split(Mid, Mid, R, 1);    // Mid has element at `pos`, R has the rest
	// Dispose(Mid); // Optional: free memory if needed
	Root := Merge(L, R);
end;

// Accesses the value at a specific position (0-based pos)
function Access(T: PNode; pos: Integer): Integer;
var
	CurrentKey: Integer;
begin
	if T = nil then
		raise Exception.Create('Index out of bounds');

	CurrentKey := GetSize(T^.left); // 0-based index

	if pos = CurrentKey then
		Access := T^.value
	else if pos < CurrentKey then
		Access := Access(T^.left, pos)
	else
		Access := Access(T^.right, pos - CurrentKey - 1);
end;

// Example Usage
var
	i: Integer;
begin
	Randomize;
	Root := nil;

	// Insert elements to create an array [10, 20, 30, 40, 50]
	Insert(Root, 0, 10);
	Insert(Root, 1, 20);
	Insert(Root, 2, 30);
	Insert(Root, 3, 40);
	Insert(Root, 4, 50);

	Writeln('Array after insertions:');
	for i := 0 to GetSize(Root) - 1 do
		Write(Access(Root, i), ' ');
	Writeln;

	// Insert 99 at position 2 (0-based) -> [10, 20, 99, 30, 40, 50]
	Insert(Root, 2, 99);
	Writeln('Array after inserting 99 at pos 2:');
	for i := 0 to GetSize(Root) - 1 do
		Write(Access(Root, i), ' ');
	Writeln;

	// Delete element at position 0 -> [20, 99, 30, 40, 50]
	Delete(Root, 0);
	Writeln('Array after deleting element at pos 0:');
	for i := 0 to GetSize(Root) - 1 do
		Write(Access(Root, i), ' ');
	Writeln;
end.
