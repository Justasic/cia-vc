from django.db import connection, models

#class FrontPageStats(models.Model):



def GetStats(counter_attrib, counter, path, sort, limit):

    cursor = connection.cursor()
    cursor.execute("""SELECT
        STAT.target_path,
        ST.title,
        ICO.path,
        STAT.""" + counter_attrib + """,
        ICO.width,
        ICO.height
    FROM stats_counters STAT FORCE INDEX (""" + counter_attrib + """)
        LEFT OUTER JOIN stats_catalog  T           ON (STAT.target_path = T.target_path)
        LEFT OUTER JOIN stats_statstarget ST       ON (T.target_path = ST.path)
        LEFT OUTER JOIN images_imageinstance ICO   ON (ICO.source_id = IF(ST.icon_id IS NOT NULL, ST.icon_id, ST.photo_id) AND ICO.thumbnail_size = 32)
        WHERE STAT.name = %s AND T.parent_path = %s
    ORDER BY STAT.""" + counter_attrib + """ """ + sort + """
    LIMIT %s""", [counter, path, limit])

    desc = cursor.description
    return [
            dict(zip([col[0] for col in desc], row))
            for row in cursor.fetchall()
    ]
