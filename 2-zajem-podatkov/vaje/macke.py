import requests
import re
import os
import csv

###############################################################################
# Najprej definirajmo nekaj pomožnih orodij za pridobivanje podatkov s spleta.
###############################################################################

# definiratje URL glavne strani bolhe za oglase z mačkami
cats_frontpage_url = 'http://www.bolha.com/zivali/male-zivali/macke/'
# mapa, v katero bomo shranili podatke
cat_directory = 'cat_data'
# ime datoteke v katero bomo shranili glavno stran
frontpage_filename = 'frontapage.html'
# ime CSV datoteke v katero bomo shranili podatke
csv_filename = 'cat_catdata.csv'


def download_url_to_string(url):
    '''This function takes a URL as argument and tries to download it
    using requests. Upon success, it returns the page contents as string.'''
    try:
        r = requests.get(url)
        # del kode, ki morda sproži napako

    except requests.exceptions.ConnectionError:
        print('nemore '+ url)
        # koda, ki se izvede pri napaki
        # dovolj je če izpišemo opozorilo in prekinemo izvajanje funkcije
        return ''
    # nadaljujemo s kodo če ni prišlo do napake
    return r.text


def save_string_to_file(text, directory, filename):
    '''Write "text" to the file "filename" located in directory "directory",
    creating "directory" if necessary. If "directory" is the empty string, use
    the current directory.'''
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding='utf-8') as file_out:
        file_out.write(text)
    return None

# Definirajte funkcijo, ki prenese glavno stran in jo shrani v datoteko.


def save_frontpage():
    '''Save "cats_frontpage_url" to the file
    "cat_directory"/"frontpage_filename"'''
    return save_string_to_file(download_url_to_string(cats_frontpage_url),cat_directory,frontpage_filename)

###############################################################################
# Po pridobitvi podatkov jih želimo obdelati.
###############################################################################


def read_file_to_string(directory, filename):
    '''Return the contents of the file "directory"/"filename" as a string.'''
    path = os.path.join(directory, filename)
    with open(path,'r', encording = 'utf-8') as dat:
        vsebina= dat.read()
    return vsebina

# Definirajte funkcijo, ki sprejme niz, ki predstavlja vsebino spletne strani,
# in ga razdeli na dele, kjer vsak del predstavlja en oglas. To storite s
# pomočjo regularnih izrazov, ki označujejo začetek in konec posameznega
# oglasa. Funkcija naj vrne seznam nizov.


def page_to_ads(page):
    '''Split "page" to a list of advertisement blocks.'''
    seznam = []
    vzorec = re.compile(r'<div class="coloumn image">(\s*?.*?\n)*?\s*?<div class="miscellaneous">')
    for ujemnaje in vzorec.finditer(page):
        seznam.append(ujemanje.group(0))
    return seznam

# Definirajte funkcijo, ki sprejme niz, ki predstavlja oglas, in izlušči
# podatke o imenu, ceni in opisu v oglasu.



def get_dict_from_ad_block(block):
    '''Build a dictionary containing the name, description and price
    of an ad block.'''
    vzorec = re.compile(
    r' <table><tr><td><a title="(?P<ime>.+?)" .*?'
    r'<div class="price">(?P<cena>.+?)</div> '
    r'</h3>(?P<opis>.*?)</div>',
    re.DOTALL)
    podatki = re.search(vzorec,block)
    slovar = podatki.groupdict()
    return slovar

# Definirajte funkcijo, ki sprejme ime in lokacijo datoteke, ki vsebuje
# besedilo spletne strani, in vrne seznam slovarjev, ki vsebujejo podatke o
# vseh oglasih strani.


def ads_from_file(directory, filename):
    '''Parse the ads in filename/directory into a dictionary list.'''
    
    return [get_dict_from_ad_block(block) for block in page_to_ads(read_file_to_string(directory, filename))]
    

###############################################################################
# Obdelane podatke želimo sedaj shraniti.
###############################################################################


def write_csv(fieldnames, rows, directory, filename):
    '''Write a CSV file to directory/filename. The fieldnames must be a list of
    strings, the rows a list of dictionaries each mapping a fieldname to a
    cell-value.'''
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w') as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)
    return None

# Definirajte funkcijo, ki sprejme neprazen seznam slovarjev, ki predstavljajo
# podatke iz oglasa mačke, in zapiše vse podatke v csv datoteko. Imena za
# stolpce [fieldnames] pridobite iz slovarjev.


def write_cat_ads_to_csv(ads):
    fieldnames = ads[0].keys()
    write_csv(fieldnames, ads,cat_directory, csv_filename)
    return None
