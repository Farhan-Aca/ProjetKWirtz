


# Librarie

import time

import pandas as pd
from selenium import webdriver
from selenium.webdriver.chrome.webdriver import WebDriver
from selenium.webdriver.common.by import By
from tqdm import tqdm


#Cette fonction a pour objectif de nous amener vers la page principal,le parametre de la fonction "elt" correspond un element d'une liste de lien.

def extraction_fbref(elt):

    driver.get(f"{elt}")
    time.sleep(7)

    try:
        accepte_cookies = driver.find_element(By.XPATH,
                                              "//*[@id='qc-cmp2-ui']/div[2]/div/button[3]")
        accepte_cookies.click()
        print("Cookies acceptés")
        time.sleep(10)
    except:
        print("Cookies non acceptés")

    time.sleep(7)

    try:
        fermeture_pub = driver.find_element(By.XPATH, f'//*[@id="fs-slot-footer-wrapper"]/button')
        fermeture_pub.click()
        print("Pub fermé")
        time.sleep(5)
    except:
        pass


#Lorsqu'on est sur la page principale calendrier et résultat d'une saison d'un championnat,nous allons récuperé tout les url des rapports de match figurant dans cette page.

def rapportdematch():
    urls = driver.find_elements(By.XPATH, '//td[@data-stat="match_report"]/a')
    return [url.get_attribute("href") for url in urls]


# Cette fonction va scrapper chaque rapport de match,elle va extraire comme information la compo,les joueurs,leur temps de jeu,s'ils jouent à domicile ou non,etc...
# On va convertir nos dictionnaire python en data frame via la librairies Pandas puis nous allons sauvegardé ces données dans un ficher xlsx en faisant un update par journée de match(loopsave).
#Les parametres correspondent au id et au nom du fichier,il y'a 380 match par saison.Lorsqu'on passe a la saison suivante,on lui incrémente la valeur 360(t)

def scrap_tablea(t,nom_du_fichier):

    id = 0 + t
    loopsave = 1
    compo_match = []

    for i in tqdm(lien_rapport_de_match):
        # pbar.update(len(team_url))
        id += 1
        loopsave += 1
        data = driver.get(i)



    #--------- Les Xpaths de chaque élement du tableau concernant la premiere equipe

        joueur_equipe_une = driver.find_elements(By.XPATH, f'((//table[contains(@id,"_summary")])[1])/tbody/tr/th/a')
        minute = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[1])/tbody//td[5]')
        position = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[1])/tbody//td[3]')

        # C'est rubrique concernant les performance
        buts = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[1])/tbody//td[6]')
        passe_decisive = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[1])/tbody//td[7]')
        penalty_marqués = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[1])/tbody//td[8]')
        penalty_tirés = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[1])/tbody//td[9]')
        tirs = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[1])/tbody//td[10]')
        tirs_cadrés = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[1])/tbody//td[11]')
        carton_jaune = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[1])/tbody//td[12]')
        carton_rouge = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[1])/tbody//td[13]')
        touches = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[1])/tbody//td[14]')
        pression = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[1])/tbody//td[15]')
        nombre_de_joueur_tacles = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[1])/tbody//td[16]')
        interception = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[1])/tbody//td[17]')
        balles_contrées = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[1])/tbody//td[18]')

        # -------Attendu
        buts_attendus = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[1])/tbody//td[19]')
        buts_attendus_sans_penalty = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[1])/tbody//td[20]')
        buts_attendus_avec_passe_decisive = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[1])/tbody//td[21]')

        # -------Action_menant_à_un_tirs
        action_menant_tirs = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[1])/tbody//td[22]')
        action_menant_buts = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[1])/tbody//td[23]')

        # -------Passes
        passes_reussies = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[1])/tbody//td[24]')
        passes_tentes = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[1])/tbody//td[25]')
        pourcentage_de_passe_reussie = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[1])/tbody//td[26]')
        passe_vers_l_avant = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[1])/tbody//td[27]')

        # -------Balle au pied
        balle_pied = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[1])/tbody//td[28]')
        progression_balle_au_pied = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[1])/tbody//td[29]')

        # -------Dribbles
        reussi = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[1])/tbody//td[30]')
        tenter = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[1])/tbody//td[31]')

        # --- Composition du match & Nom du club à domicile
        club_domicile = driver.find_elements(By.XPATH, '(//div[@class="th"][1])[1]')
        composition_domicile = driver.find_elements(By.XPATH, "(//tbody//th[@colspan = '2'])[1]")

        # -----------------------------Xpath de la deuxieme équipe

        joueur_equipe_deux = driver.find_elements(By.XPATH, f'((//table[contains(@id,"_summary")])[2])/tbody/tr/th/a')
        minute2 = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[2])/tbody//td[5]')
        position2 = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[2])/tbody//td[3]')

        #-------Performance
        buts2 = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[2])/tbody//td[6]')
        passe_decisive2 = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[2])/tbody//td[7]')
        penalty_marqués2 = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[2])/tbody//td[8]')
        penalty_tirés2 = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[2])/tbody//td[9]')
        tirs2 = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[2])/tbody//td[10]')
        tirs_cadrés2 = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[2])/tbody//td[11]')
        carton_jaune2 = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[2])/tbody//td[12]')
        carton_rouge2 = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[2])/tbody//td[13]')
        touches2 = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[2])/tbody//td[14]')
        pression2 = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[2])/tbody//td[15]')
        nombre_de_joueur_tacles2 = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[2])/tbody//td[16]')
        interception2 = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[2])/tbody//td[17]')
        balles_contrées2 = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[2])/tbody//td[18]')

        # -------Attendu
        buts_attendus2 = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[2])/tbody//td[19]')
        buts_attendus_sans_penalty2 = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[2])/tbody//td[20]')
        buts_attendus_avec_passe_decisive2 = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[2])/tbody//td[21]')

        # -------Action menant à un tirs
        action_menant_tirs2 = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[2])/tbody//td[22]')
        action_menant_buts2 = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[2])/tbody//td[23]')

        # -------Passes
        passes_reussies2 = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[2])/tbody//td[24]')
        passes_tentes2 = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[2])/tbody//td[25]')
        pourcentage_de_passe_reussie2 = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[2])/tbody//td[26]')
        passe_vers_l_avant2 = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[2])/tbody//td[27]')

        # -------Balle au pied
        balle_pied2 = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[2])/tbody//td[28]')
        progression_balle_au_pied2 = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[2])/tbody//td[29]')

        # -------Dribbles
        reussi2 = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[2])/tbody//td[30]')
        tenter2 = driver.find_elements(By.XPATH, '((//table[contains(@id,"_summary")])[2])/tbody//td[31]')

        # --- Composition du match & Nom du club à l'exterieur

        composition_exterieur = driver.find_elements(By.XPATH, "(//tbody//th[@colspan = '2'])[3]")
        club_a_lexterieur = driver.find_elements(By.XPATH, '(//div[@class="th"][3])[1]')

        for j in range(len(joueur_equipe_une)):  # Faut separé les deux tableaux

            data_test = {'id': id, 'Club': club_domicile[0].text, 'Equipe à domicile': joueur_equipe_une[j].text,
                         'Position': position[j].text, 'Minute joué': minute[j].text,
                         'buts': buts[j].text, 'passe décisive': passe_decisive[j].text, 'penalty marqué': penalty_marqués[j].text,
                         'penalty tiré': penalty_tirés[j].text, 'tirs': tirs[j].text, 'tirs cadrés': tirs_cadrés[j].text,
                         'carton jaune': carton_jaune[j].text,
                         'carton rouge': carton_rouge[j].text, 'touches': touches[j].text, 'pression': pression[j].text, 'Nombre de joueur tacles': nombre_de_joueur_tacles[j].text,
                         'interception': interception[j].text,
                         'balles contrées': balles_contrées[j].text, 'buts attendus': buts_attendus[j].text, 'buts attendus sans penalty': buts_attendus_sans_penalty[j].text,
                         'buts attendus avec passe decisive': buts_attendus_avec_passe_decisive[j].text, 'action menant tirs': action_menant_tirs[j].text, 'action menant buts': action_menant_buts[j].text,
                         'passes reussies': passes_reussies[j].text, 'passes tentes': passes_tentes[j].text, 'passes reussies%': pourcentage_de_passe_reussie[j].text, 'passe avant': passe_vers_l_avant[j].text,
                         'balle au pied': balle_pied[j].text, 'progession balle au pied': progression_balle_au_pied[j].text,
                         'Dribble reussi': reussi[j].text, 'Dribble tenter': tenter[j].text,'composition du match': composition_domicile[0].text}
            compo_match.append(data_test)

        compo_match.append({"Equipe à domicile": ""})

        for g in range(len(joueur_equipe_deux)):

            data_test2 = {'id': id, 'Club': club_a_lexterieur[0].text, 'Equipe à domicile': joueur_equipe_deux[g].text,
                         'Position': position2[g].text, 'Minute joué': minute2[g].text,
                         'buts': buts2[g].text, 'passe décisive': passe_decisive2[g].text, 'penalty marqué': penalty_marqués2[g].text,
                         'penalty tiré': penalty_tirés2[g].text, 'tirs': tirs2[g].text, 'tirs cadrés': tirs_cadrés2[g].text,
                         'carton jaune': carton_jaune2[g].text,
                         'carton rouge': carton_rouge2[g].text, 'touches': touches2[g].text, 'pression': pression2[g].text, 'Nombre de joueur tacles': nombre_de_joueur_tacles2[g].text,
                         'interception': interception2[g].text,
                         'balles contrées': balles_contrées2[g].text, 'buts attendus': buts_attendus2[g].text, 'buts attendus sans penalty': buts_attendus_sans_penalty2[g].text,
                         'buts attendus avec passe decisive': buts_attendus_avec_passe_decisive2[g].text, 'action menant tirs': action_menant_tirs2[g].text, 'action menant buts': action_menant_buts2[g].text,
                         'passes reussies': passes_reussies2[g].text, 'passes tentes': passes_tentes2[g].text, 'passes reussies%': pourcentage_de_passe_reussie2[g].text, 'passe avant': passe_vers_l_avant2[g].text,
                         'balle au pied': balle_pied2[g].text, 'progession balle au pied': progression_balle_au_pied2[g].text,
                         'Dribble reussi': reussi2[g].text, 'Dribble tenter': tenter2[g].text,'composition du match': composition_exterieur[0].text}
            compo_match.append(data_test2)

        compo_match.append({"Equipe à l'exterieur": ""})

        if loopsave == 11:
            df_data = pd.DataFrame(compo_match)
            df_data.to_excel(f'{nom_du_fichier}', index=False)
            loopsave = 1

    return compo_match



PATH = "C:\SeleniumDrivers\chromedriver.exe"
driver: WebDriver = webdriver.Chrome(PATH)

liste_lien_pageprincipal = ["https://fbref.com/fr/comps/9/2019-2020/calendrier/Calendrier-et-resultats-2019-2020-Premier-League","https://fbref.com/fr/comps/9/2020-2021/calendrier/Calendrier-et-resultats-2020-2021-Premier-League",
 "https://fbref.com/fr/comps/9/2021-2022/calendrier/Calendrier-et-resultats-2021-2022-Premier-League"]


z=0
# On boucle sur nos fonctions pour qu'on puisse répeter trois la meme action
for elt in liste_lien_pageprincipal :

    z += 1
    extraction_fbref(elt)
    lien_rapport_de_match = rapportdematch()

    #Pour chaque 
    if z == 1:
        t = 0
        nom_du_fichier = "Data_saison2019_2020.xlsx"
    elif z == 2:
        t = 380
        nom_du_fichier = "Data_saison2020_2021.xlsx"
    else :
        t = 760
        nom_du_fichier = "Data_saison2021_22.xlsx"

    scrap_tablea(t,nom_du_fichier)
