import base64

def get_index_html_content():
    with open('index.html') as f:
        return f.read()

def get_base64():
    with open("photo.jpg") as f:
        return base64.b64encode(f.read())


def get_script_content():
    with open('cv.js') as f:
        return "\n".join(['var img = "{}";'.format('data:image/jpg;base64,' + get_base64()),
                          f.read()])
def get_css_content():
    with open('cv.css') as f:
        return f.read()

def update_index_html():
    content = get_index_html_content()
    content = content.replace('<link rel="stylesheet" href="cv.css">',
                              '<style>{}</style>'.format(get_css_content()))
    return content.replace('<script type="text/javascript" src="cv.js"></script>',
                           '<script type="text/javascript">{}</script>'.format(get_script_content()))

if __name__ == '__main__':
    with open('cv.html', 'w') as f:
        f.write(update_index_html())
