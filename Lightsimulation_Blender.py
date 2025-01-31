

# -*- coding: utf-8 -*-
"""
Created on Mon Nov  4 14:26:09 2024

@author: theisen
"""

import os
import bpy
import pytz
import requests
import zipfile
import pandas as pd
from datetime import datetime, timedelta


# Set file paths
ground_obj_file_path = r"D:\Marius\R\output\lidr_ground_963.ply"
folder_trunk = r"D:\Marius\R\Wood_middle\OBJ\OBJ_Mtl"
folder_branch = r"D:\Marius\R\Branch_middle\OBJ\OBJ_Mtl"
folder_leaf = r"D:\Marius\R\Leaf_middle\OBJ\OBJ_Mtl"

# Scene cleanup
bpy.ops.object.select_all(action='SELECT')
bpy.ops.object.delete(use_global=False, confirm=False)

# Import and process trunks
for file in os.listdir(folder_trunk):
    if file.endswith(".obj"):
        full_path = os.path.join(folder_trunk, file)
        bpy.ops.wm.obj_import(filepath=full_path, forward_axis='Y', up_axis='Z')
        imported_obj = bpy.context.selected_objects[0]  # Das zuletzt importierte Objekt
        imported_obj.name = "Trunk_" + imported_obj.name  # Füge Präfix hinzu

# Apply normals consistency for trunks and hide them from the camera
for obj in bpy.context.scene.objects:
    if obj.name.startswith("Trunk_"):
        bpy.context.view_layer.objects.active = obj  # Setze das aktuelle Objekt
        obj.select_set(True)
        bpy.ops.object.mode_set(mode='EDIT')
        bpy.ops.mesh.select_all(action='SELECT')
        bpy.ops.mesh.normals_make_consistent(inside=False)
        bpy.ops.object.mode_set(mode='OBJECT')
        obj.visible_camera = False

folder_branch = r"D:\Marius\R\Branch_middle\OBJ\OBJ_Mtl"
# Import and process branches
for file in os.listdir(folder_branch):
    if file.endswith(".obj"):
        full_path = os.path.join(folder_branch, file)
        bpy.ops.wm.obj_import(filepath=full_path, forward_axis='Y', up_axis='Z')
        imported_obj = bpy.context.selected_objects[0]  # Das zuletzt importierte Objekt
        imported_obj.name = "Branch_" + imported_obj.name  # Füge Präfix hinzu

# Apply normals consistency for branches and hide them from the camera
for obj in bpy.context.scene.objects:
    if obj.name.startswith("Branch_"):
        bpy.context.view_layer.objects.active = obj  # Setze das aktuelle Objekt
        obj.select_set(True)
        bpy.ops.object.mode_set(mode='EDIT')
        bpy.ops.mesh.select_all(action='SELECT')
        bpy.ops.mesh.normals_make_consistent(inside=False)
        bpy.ops.object.mode_set(mode='OBJECT')
        obj.visible_camera = False


# Import and process Leaf
for file in os.listdir(folder_leaf):
    if file.endswith(".obj"):
        full_path = os.path.join(folder_leaf, file)
        bpy.ops.wm.obj_import(filepath=full_path, forward_axis='Y', up_axis='Z')
        imported_obj = bpy.context.selected_objects[0]  # Das zuletzt importierte Objekt
        imported_obj.name = "Leaf_" + imported_obj.name  # Füge Präfix hinzu

# Apply normals consistency for Leafs and hide them from the camera
for obj in bpy.context.scene.objects:
    if obj.name.startswith("Leaf_"):
        bpy.context.view_layer.objects.active = obj  # Setze das aktuelle Objekt
        obj.select_set(True)
        bpy.ops.object.mode_set(mode='EDIT')
        bpy.ops.mesh.select_all(action='SELECT')
        bpy.ops.mesh.normals_make_consistent(inside=False)
        bpy.ops.object.mode_set(mode='OBJECT')
        obj.visible_camera = False


# Import DEM
bpy.ops.wm.ply_import(filepath=ground_obj_file_path, forward_axis='Y', up_axis='Z')
bpy.ops.object.select_all(action='DESELECT')

# Add and configure camera
bpy.ops.object.camera_add()
camera = bpy.context.object
camera.data.type = 'ORTHO'
camera.data.ortho_scale = 350
camera.location = (0, 0, 100)
camera.rotation_euler = (0, 0, 0)
bpy.context.scene.camera = camera

# Enable sun position add-on
addon_name = "sun_position"
if not bpy.context.preferences.addons.get(addon_name):
    bpy.ops.preferences.addon_enable(module=addon_name)

# Set up sun position properties
sun_position = bpy.context.scene.sun_pos_properties
sun_position.latitude = 48.2682664633752
sun_position.longitude = 7.87815710988247
bpy.context.scene.sun_pos_properties.sun_distance = 500

# Add sun light and configure it for sun position add-on
sun_light_data = bpy.data.lights.new(name="Sun_Light", type='SUN')
sun_light_object = bpy.data.objects.new(name="Sun_Light_Object", object_data=sun_light_data)
bpy.context.collection.objects.link(sun_light_object)
#sun_light_data.energy = 6.0
sun_light_data.angle = 0.000872665  # 5 degrees
bpy.context.scene.sun_pos_properties.sun_object = bpy.data.objects["Sun_Light_Object"]

# Set rendering engine and device
bpy.context.scene.render.engine = 'CYCLES'
bpy.context.scene.cycles.device = 'GPU'



sky_texture = bpy.context.scene.world.node_tree.nodes.new("ShaderNodeTexSky")
bg = bpy.context.scene.world.node_tree.nodes["Background"]
bpy.context.scene.world.node_tree.links.new(bg.inputs["Color"], sky_texture.outputs["Color"])
bpy.data.worlds["World"].node_tree.nodes["Sky Texture"].sun_disc = False
bpy.context.scene.sun_pos_properties.sky_texture = "Sky Texture"



# URL der Zip-Datei für Lahr
#rl = "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/10_minutes/solar/recent/10minutenwerte_SOLAR_02812_akt.zip"
# URL der Zip-Datei für Freiburg
url = "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/10_minutes/solar/recent/10minutenwerte_SOLAR_01443_akt.zip"
# Zielverzeichnis
destination_folder = r"D:\Marius\Blender\DWD_export\10min"
zip_file_path = os.path.join(destination_folder, "10minutenwerte_SOLAR_01443_akt.zip")
extract_folder = destination_folder  # Entpacken in dasselbe Verzeichnis

# Verzeichnis erstellen, falls es nicht existiert
os.makedirs(destination_folder, exist_ok=True)

# 1. Herunterladen der Datei
print("Lade Datei herunter")
response = requests.get(url)
with open(zip_file_path, "wb") as f:
    f.write(response.content)

# 2. Entpacken der ZIP-Datei
print("Entpacke ZIP-Datei")
with zipfile.ZipFile(zip_file_path, "r") as zip_ref:
    zip_ref.extractall(extract_folder)

# Alle Dateien im Zielordner auflisten
dateien = os.listdir(destination_folder)

# Nur .txt-Dateien filtern und den kompletten Pfad erhalten
txt_dateien_pfade = [os.path.join(destination_folder, datei) for datei in dateien if datei.endswith(".txt")]

   
    
    
# Die erste .txt-Datei einlesen
if txt_dateien_pfade:
    data_file_path = txt_dateien_pfade[0]  # Die erste .txt-Datei auswählen
    Lahr_10min = pd.read_csv(data_file_path, sep=";", encoding="latin1")  # Datei einlesen


    # Spalte "MESS_DATUM" in datetime konvertieren
    Lahr_10min['datetime'] = pd.to_datetime(Lahr_10min['MESS_DATUM'], format='%Y%m%d%H%M')
    # Setzen der Zeitzone auf UTC
    Lahr_10min['datetime'] = Lahr_10min['datetime'].dt.tz_localize('UTC')
    # Umwandeln in die Zeitzone 'Europe/Berlin' (MEZ/MESZ), Sommerzeit wird automatisch berücksichtigt
    Lahr_10min['datetime'] = Lahr_10min['datetime'].dt.tz_convert('Europe/Berlin')
    


#Stündliche Duett DAten Laden und entpacken
url = "https://opendata.dwd.de/climate_environment/CDC/derived_germany/climate/hourly/duett/radiation_global/recent/stundenwerte_duett_FG_18219_akt.zip"

# Zielverzeichnis
destination_folder_duett = r"D:\Marius\Blender\DWD_export\duett"
zip_file_path_duett = os.path.join(destination_folder_duett, "stundenwerte_duett_FG_18219_akt.zip")
extract_folder_duett = destination_folder_duett  # Entpacken in dasselbe Verzeichnis

# Verzeichnis erstellen, falls es nicht existiert
os.makedirs(destination_folder_duett, exist_ok=True)

# 1. Herunterladen der Datei
print("Lade Datei herunter")
response = requests.get(url)
with open(zip_file_path_duett, "wb") as f:
    f.write(response.content)

# 2. Entpacken der ZIP-Datei
print("Entpacke ZIP-Datei")
with zipfile.ZipFile(zip_file_path_duett, "r") as zip_ref:
    zip_ref.extractall(extract_folder_duett)

# Alle Dateien im Zielordner auflisten
dateien_duett = os.listdir(destination_folder_duett)

# Nur .txt-Dateien filtern und den kompletten Pfad erhalten
#txt_dateien_pfade_duett = [os.path.join(destination_folder_duett, datei) for datei in dateien_duett if datei.(".txt")]
txt_dateien_pfade_duett = [os.path.join(destination_folder_duett, datei) for datei in dateien_duett if "produkt_fg_duett" in datei]



# Die erste .txt-Datei einlesen
if txt_dateien_pfade_duett:
    data_file_path_duett = txt_dateien_pfade_duett[0]  # Die erste .txt-Datei auswählen
    Lahr_1h = pd.read_csv(data_file_path_duett, sep=";", encoding="latin1")  # Datei einlesen


    # Spalte "MESS_DATUM" in datetime konvertieren
    Lahr_1h['datetime'] = pd.to_datetime(Lahr_1h['MESS_DATUM'], format='%Y%m%d%H')
    # Setzen der Zeitzone auf UTC
    Lahr_1h['datetime'] = Lahr_1h['datetime'].dt.tz_localize('UTC')
    # Umwandeln in die Zeitzone 'Europe/Berlin' (MEZ/MESZ), Sommerzeit wird automatisch berücksichtigt
    Lahr_1h['datetime'] = Lahr_1h['datetime'].dt.tz_convert('Europe/Berlin')








# Sonnenobjekt auswählen, sodass darauf innerhalb der folgenden Schleife zugegriffen werden kann
sun_object_name = "Sun_Light_Object"

# Alle Objekte in der Szene abwählen
for obj in bpy.data.objects:
    obj.select_set(False)

# Sonne auswählen und aktiv setzen
if sun_object_name in bpy.data.objects:
    sun_object = bpy.data.objects[sun_object_name]
    sun_object.select_set(True)  # Objekt auswählen
    bpy.context.view_layer.objects.active = sun_object  # Aktiv setzen
    print(f"'{sun_object_name}' wurde ausgewählt.")
else:
    print(f"Objekt mit dem Namen '{sun_object_name}' existiert nicht.")




def render_sun_movement(start_date_str, start_time_str, end_date_str, end_time_str, interval_minutes, output_folder):
    from datetime import datetime, timedelta
    import os
    import pytz
    import bpy

    # Umwandlung der Eingabewerte in datetime-Objekte
    start_datetime = datetime.strptime(f"{start_date_str} {start_time_str}", "%d.%m.%Y %H:%M")
    end_datetime = datetime.strptime(f"{end_date_str} {end_time_str}", "%d.%m.%Y %H:%M")

    # Zielverzeichnis erstellen, falls es nicht existiert
    os.makedirs(output_folder, exist_ok=True)

    # Zeitzone für Deutschland festlegen
    german_tz = pytz.timezone('Europe/Berlin')

    # Lokalisieren der Datumsangaben in der deutschen Zeitzone (nur, wenn noch keine Zeitzone gesetzt ist)
    start_datetime = german_tz.localize(start_datetime) if start_datetime.tzinfo is None else start_datetime
    end_datetime = german_tz.localize(end_datetime) if end_datetime.tzinfo is None else end_datetime

    current_datetime = start_datetime

    while current_datetime <= end_datetime:
        # UTC-Offset basierend auf der aktuellen Zeit berechnen
        utc_offset = int(current_datetime.utcoffset().total_seconds() // 3600)

        # Suche den passenden Wert in der Lahr-Daten Tabelle (GS_10)
        closest_row_10min = Lahr_10min.iloc[(Lahr_10min['datetime'] - current_datetime).abs().argmin()]
        ds_10_value = closest_row_10min['DS_10']  # Wert der diffusen Strahlung
        ds_10_watt = ds_10_value / 600 * 10000  # Umrechnung in W/m²

        if ds_10_value == -999:  # Fehlender Wert
            ds_10_watt = 1

        # Setze den Wert von Background in W/m²
        bpy.data.worlds["World"].node_tree.nodes["Background"].inputs[1].default_value = ds_10_watt

        # Direkte Strahlung berechnen
        gs_10_value = closest_row_10min['GS_10']  # Wert der Globalstrahlung
        gs_10_watt = gs_10_value / 600 * 10000  # Umrechnung in W/m²

        if ds_10_value != -999:  # Direkte Strahlung berechnen
            direct_10_watt = gs_10_watt - ds_10_watt
        else:
            direct_10_watt = gs_10_watt  # Globalstrahlung, falls keine diffuse Strahlung vorhanden

        # Falls GS_10 = -999, ersetze durch den Wert aus der stündlichen Tabelle
        if gs_10_value == -999:
            closest_row_1h = Lahr_1h.iloc[(Lahr_1h['datetime'] - current_datetime).abs().argmin()]
            direct_10_watt = closest_row_1h['FG_DUETT']  # Wert aus FG_DUETT verwenden

            if direct_10_watt == -999:  # Fehlender Wert
                direct_10_watt = 0

        bpy.context.object.data.energy = direct_10_watt

        # Aktualisiere die Sonnenposition
        bpy.context.scene.sun_pos_properties.year = current_datetime.year
        bpy.context.scene.sun_pos_properties.month = current_datetime.month
        bpy.context.scene.sun_pos_properties.day = current_datetime.day
        bpy.context.scene.sun_pos_properties.time = current_datetime.hour + current_datetime.minute / 60.0
        bpy.context.scene.sun_pos_properties.UTC_zone = utc_offset

        # Dateiname mit Zeitstempel
        bpy.context.scene.render.filepath = os.path.join(
            output_folder, f"{current_datetime.strftime('%Y%m%d_%H%M')}.png"
        )

        # Bild rendern und speichern
        bpy.ops.render.render(write_still=True)

        # Zeit um das Intervall erhöhen
        current_datetime += timedelta(minutes=interval_minutes)




# Parameterangaben als Strings
start_date_str = "25.06.2024"     # Startdatum im Format 'dd.mm.yyyy'
start_time_str = "04:00"              # Startzeit im Format 'hh:mm'
end_date_str = "25.06.2024"           # Enddatum im Format 'dd.mm.yyyy'
end_time_str = "21:00"                # Endzeit im Format 'hh:mm'
interval_minutes = 10                 # Intervall in Minuten

#output_folder = r"F:\ARS\Masterarbeit_Theisen\Blender_export\Tiffs90"  # Pfad zum Ausgabeordner
output_folder= r"D:\Marius\Blender\blender_final_export\27112024"
os.makedirs(output_folder, exist_ok=True)
# Render-Funktion aufrufen
render_sun_movement(start_date_str, start_time_str, end_date_str, end_time_str, interval_minutes, output_folder)

