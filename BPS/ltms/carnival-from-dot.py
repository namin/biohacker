import pydot

val_colors = {'lavender':'+', 'mistyrose':'-', None: '?'}
val_arrowheads = {'"vee"':'+', '"tee"':'-'}

def read_graph(filename):
    graphs = pydot.graph_from_dot_file(filename)
    return graphs[0]

def not_destination(n, g):
    for e in g.get_edges():
        if e.get_destination()==n.get_name():
            return False
    return True

def print_node(n, g):
    val = val_colors[n.get_fillcolor()]
    name = n.get_name()
    measured = n.get_shape()=='doublecircle'
    top = not_destination(n, g)
    measured_opt = ":measured? t" if measured else ""
    top_opt = ":top? t" if top else ""
    print("(node %s %s %s %s)" % (val, name, measured_opt, top_opt))

def print_edge(e, g):
    val = val_arrowheads[e.get_arrowhead()]
    src = e.get_source()
    dst = e.get_destination()
    print("(edge %s %s %s)" % (val, src, dst))

def print_graph(g):
    for n in g.get_nodes(): print_node(n, g)
    for e in g.get_edges(): print_edge(e, g)

def main():
    import sys
    filename = sys.argv[1]
    g = read_graph(filename)
    print_graph(g)

main()
