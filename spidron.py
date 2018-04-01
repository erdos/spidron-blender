# run script with: ALT+P

import bpy
import bmesh
from math import sin, cos, sqrt, pi, atan2, asin
from bpy.props import FloatProperty, BoolProperty, FloatVectorProperty, IntProperty

bl_info = \
    {
        "name": "Spidron Generator",
        "author": "Janos Erdos Jr. <erdosj@gmail.com>",
        "version": (1, 0, 0),
        "blender": (2, 7, 9),
        "location": "Add > Mesh > Add Spidron",
        "description": "Generate a dynamic spidron shape",
        "warning": "",
        "wiki_url": "",
        "tracker_url": "",
        "category": "Add Mesh",
    }


def radians(x):
    return x * pi / 180


def rotateZ(p, x):
    return (p[0] * cos(x) - p[1] * sin(x), p[0] * sin(x) + p[1] * cos(x), p[2])


class Spidron:

    def __init__(self, a=pi/6, c=pi/3, n=6, f=radians(60)):
        self.a, self.c, self.n, self.f = float(a), float(c), int(n), float(f)
        self.l = 12 # nr of levels to draw

    @classmethod
    def FromMap(h):
        return Spidron(a=h['alpha'], c=h['gamma'], n=h['sides'])

    def hash(self):
        return hash(tuple([self.a, self.c, self.n, self.f, self.l]))

    def skew_polygon(self, n, f, rot, sca):
        x, y, z = sin(pi/n)*cos(f)*sca,  cos(pi/n)*cos(f)*sca, sin(pi / n) * sin(f) * sca
        return [rotateZ([x, y, (z, -z)[i % 2]], pi * 2 / n * i + rot) for i in range(0, n)]

    def f_angle(self, f):
        n, a, c = self.n, self.a, self.c
        return abs(
            asin(sqrt((2*(pow(sin(f)*cos(a), 2) + 1)*pow(cos(pi/n), 2) +
        2 * sqrt(-2 * pow(sin(f) * cos(pi/n), 2) + cos(c) + 1)*sqrt(2)*sin(pi/n)*cos(a) -
        2 * pow(cos(a), 2) - cos(c) - 1) / ((-cos(c) + 1)*cos(pi/n)))))*(1, -1)[f > 0]


    def rot_angle(self, f):
        f, n, c, b, sn, cn = abs(f), self.n, self.c, self.a, sin(pi / self.n), cos(pi / self.n)
        return (pi/n + atan2(-2 * sqrt(-1/4 * pow(2 * sn * cos(b) -
      sqrt(-4 * pow(sin(f) * cn, 2) + 2 * cos(c) + 2), 2) / pow(sin(b)*cos(f)*cn, 2) +
      1) * pow(sin(b), 2) * sin(f) * sn / sin(pi - 2 * b) +
      (2 * sin(b) * sn * cos(b) / sin(pi - 2 * b) - sn) * cos(f),
      cos(f)*cn + (2*sn*cos(b) - sqrt(-4*pow(sin(f)*cn, 2) + 2*cos(c) + 2))*sin(b)*sn/(sin(pi - 2*b)*cos(f)*cn)))

    def build(self):
        a, b, c, n = self.a, self.a, self.c, self.n
        ab = 2*sin(pi/n)
        ac, bc = ab*sin(b)/sin(pi-a-b), ab*sin(a)/sin(pi-a-b)
        cd = sqrt(ac*ac + bc*bc - 2 * ac * bc * cos(c))

        scarat = cd/ab

        fn, sca, rot, poly = self.f, 1, 0, []

        poly, poly_indices = self.skew_polygon(n, fn, rot, sca), []

        for j in range(0, self.l):

            sca *= scarat
            rot += self.rot_angle(fn)
            fn = self.f_angle(fn)

            poly.extend(self.skew_polygon(n, fn, rot, sca))

            for i in range(0, n):
                poly_indices.append([(j+1)*n+i, j*n+(i+1) % n, (j+1)*n+(i+1) % n])
                poly_indices.append([j*n+i, j*n+(i+1) % n, (j+1)*n+i])

        return (poly, poly_indices)


class SpidronProperties():
    p_a = FloatProperty(name="alpha angle", default=30, min=0, max=90)
    p_c = FloatProperty(name="gamma angle", default=60, min=0, max=180)
    p_f = FloatProperty(name="f0-angle", default=35, min=0, max=90)
    p_n = IntProperty(name="sides", default=6, min=4)
    p_l = IntProperty(name="levels", default=8, min=1)


class SpidronGenerator(bpy.types.Operator, SpidronProperties):

    bl_idname = "mesh.primitive_spidron_add"
    bl_label = "Add Spidron"
    bl_options = {'REGISTER', 'UNDO'}

    def execute(self, context):

        spidron = Spidron(a=radians(self.p_a), c=radians(self.p_c), n=self.p_n, f=radians(self.p_f))
        verts, faces = spidron.build()

        me = bpy.data.meshes.new('Spidron')
        me.from_pydata(verts, [], faces)
        me.update(calc_edges=True)

        ob = bpy.data.objects.new("Spidron", me)
        ob['f-angle'] = self.p_f
        ob['alpha'] = self.p_a
        ob['gamma'] = self.p_c
        ob['sides'] = self.p_n
        ob['spidron'] = str(spidron.hash())

        bpy.context.scene.objects.link(ob)
        bpy.context.scene.objects.active = ob
        ob.select = True
        ob.location = bpy.context.scene.cursor_location

        return {'FINISHED'}

    def invoke(self, context, event):
        wm = context.window_manager
        return wm.invoke_props_dialog(self)


class SpidronPanel(bpy.types.Panel):
    bl_idname = "OBJECT_PT_spidron"
    bl_label = "Spidron"
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "object"
    # bl_options = {'DEFAULT_CLOSED'}

    @classmethod
    def poll(cls, context):
        return context.object.name.startswith("Spidron")

    def draw(self, context):
        layout = self.layout
        obj = context.object

        box = layout.box()
        box.label("folding")
        box.prop(obj, '["f-angle"]')

        box = layout.box()
        box.label("triangles")

        row = box.row()
        row.label("even triangles")
        row.prop(obj, '["alpha"]')

        row = box.row()
        row.label("odd triangles")
        row.prop(obj, '["gamma"]')

        # TODO: maybe make number of levels adjustable?


def menu_func(self, context):
    self.layout.operator(SpidronGenerator.bl_idname, icon='MESH_CUBE')


def spidron_update(context):
    if not bpy.data.objects.is_updated:
        return

    for ob in bpy.data.objects:

        if not (isinstance(ob, bpy.types.bpy_struct)):
            continue

        if 'f-angle' not in ob.keys():
            continue

        if not ob.is_updated:
            continue

        spidron = Spidron(a=radians(ob['alpha']), c=radians(ob['gamma']), n=ob['sides'], f=radians(ob['f-angle']))

        if ob['spidron'] == str(spidron.hash()):
            continue

        verts, faces = spidron.build()

        for i in range(0, len(verts)):
            ob.data.vertices[i].co.x = verts[i][0]
            ob.data.vertices[i].co.y = verts[i][1]
            ob.data.vertices[i].co.z = verts[i][2]

        ob['spidron'] = str(spidron.hash())
        ob.update_tag(refresh={'DATA'})


def register():
    bpy.utils.register_class(SpidronGenerator)
    bpy.utils.register_class(SpidronPanel)
    bpy.types.INFO_MT_mesh_add.append(menu_func)
    bpy.app.handlers.scene_update_pre.append(spidron_update)


def unregister():
    bpy.utils.unregister_class(SpidronGenerator)
    bpy.utils.unregister_class(SpidronPanel)
    bpy.types.INFO_MT_mesh_add.remove(menu_func)
    bpy.app.handlers.scene_update_pre.remove(spidron_update)


if __name__ == "__main__":
    register()
