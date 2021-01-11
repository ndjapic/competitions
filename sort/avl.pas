program tsort;
const
    maxt = 1000 * 1000;
type
    tdata = int32;
    tavltree = record
        root, size: int32;
        nodes: array [0 .. maxt] of record
            data: tdata;
            freq, parent: int32;
            childs: array [0 .. 1] of int32;
            height, direction: int8;
        end;
    end;
var
    t, i, j, u, v, freq: int32;
    a: array [1 .. maxt] of tdata;
    tree: tavltree;

procedure createtree;
begin
    tree.root := 0;
    tree.size := 0;
    tree.nodes[0].height := 0;
end;

procedure connect(parent, child: int32; direction: int8);
begin
    if parent = 0 then
        tree.root := child
    else
        tree.nodes[parent].childs[direction] := child;
    tree.nodes[child].parent := parent;
    tree.nodes[child].direction := direction;
end;

procedure appendnode(data: tdata; parent: int32; direction: int8);
begin
    inc(tree.size);
    tree.nodes[tree.size].data := data;
    tree.nodes[tree.size].freq := 1;
    tree.nodes[tree.size].childs[0] := 0;
    tree.nodes[tree.size].childs[1] := 0;
    tree.nodes[tree.size].height := 1;
    connect(parent, tree.size, direction);
end;

procedure rotate(pivot: int32);
var
    root, child, parent: int32;
    direction: int8;
begin
    assert((pivot <> 0) and (pivot <> tree.root));
    root := tree.nodes[pivot].parent;
    parent := tree.nodes[root].parent;
    direction := 1 - tree.nodes[pivot].direction;
    child := tree.nodes[pivot].childs[direction];
    connect(root, child, 1 - direction);
    connect(parent, pivot, tree.nodes[root].direction);
    connect(pivot, root, direction);
end;

procedure searchappend(data: tdata; root: int32);
var
    dif, child, parent, sibling: int32;
    direction, dir2: int8;
begin
    if root = 0 then
        appendnode(data, 0, 0)
    else begin
        dif := data - tree.nodes[root].data;
        if dif = 0 then
            inc(tree.nodes[root].freq)
        else begin
            direction := ord(dif > 0);
            child := tree.nodes[root].childs[direction];
            if child = 0 then
                appendnode(data, root, direction)
            else begin
                searchappend(data, child);
                if tree.nodes[child].height =
                   tree.nodes[root].height then
                    inc(tree.nodes[root].height);
                parent := tree.nodes[root].parent;
                if parent <> 0 then begin
                    dir2 := tree.nodes[root].direction;
                    sibling := tree.nodes[parent].childs[1 - dir2];
                    if tree.nodes[root].height -
                       tree.nodes[sibling].height >= 2 then begin
                        if dir2 <> direction then rotate(child);
                        rotate(root);
                    end;
                end;
            end;
        end;
    end;
end;

function firstnode(root: int32): int32;
var
    u, v: int32;
begin
    u := root;
    v := tree.nodes[u].childs[0];
    while v <> 0 do begin
        u := v;
        v := tree.nodes[u].childs[0];
    end;
    firstnode := u;
end;

function nextnode(u: int32): int32;
begin
    v := tree.nodes[u].childs[1];
    if v <> 0 then
        u := firstnode(v)
    else begin
        while (u <> 0) and (tree.nodes[u].direction = 1) do
            u := tree.nodes[u].parent;
        if u <> 0 then
            u := tree.nodes[u].parent;
    end;
    nextnode := u;
end;


begin
    readln(t);
    for i := 1 to t do readln(a[i]);

    createtree;
    for i := 1 to t do searchappend(a[i], tree.root);

    u := firstnode(tree.root);
    i := 0;
    while u <> 0 do begin

        freq := tree.nodes[u].freq;
        for j := i + 1 to i + freq do
            a[j] := tree.nodes[u].data;
        inc(i, freq);
        u := nextnode(u);

    end;

    for i := 1 to t do writeln(a[i]);
end.
