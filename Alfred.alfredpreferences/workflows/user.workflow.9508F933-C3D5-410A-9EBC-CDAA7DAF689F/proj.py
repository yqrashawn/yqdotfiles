import os
import alp
from alp.item import Item as I
import codecs
import json


def find_projects():
    q = alp.args()[0] if len(alp.args()) else ""

    session_path = []
    st3_sesh = os.path.expanduser("~/Library/Application Support/Sublime Text 3/Local/Session.sublime_session")
    st2_sesh = os.path.expanduser("~/Library/Application Support/Sublime Text 2/Settings/Session.sublime_session")
    if os.path.exists(st3_sesh):
        session_path.append(st3_sesh)
    if os.path.exists(st2_sesh):
        session_path.append(st2_sesh)
    if not len(session_path):
        alp.feedback(I(title="No Sublime Installation",
                        subtitle="Sublime Text 2 or 3 is required.",
                        valid=False))
        return

    projectNames = []
    for p in session_path:
        with codecs.open(p, "r", "utf-8") as f:
            session_json = f.read()
        session_json = session_json.replace('\t', '')
        projects = json.loads(session_json)["workspaces"]["recent_workspaces"]
        for project in projects:
            # projPath = project
            (projPath, projFile) = os.path.split(project)
            (projTitle, _) = projFile.rsplit(".", 1)
            if "2" in p:
                projTitle += " (ST2)"
                v_arg = project
            elif "3" in p:
                projTitle += " (ST3)"
                v_arg = project

            if os.path.exists(project):
                projectNames.append([project, projTitle, v_arg])

    items = []
    if q == "":
        for path, title, arg in projectNames:
            items.append(I(title=title,
                            subtitle=path,
                            arg=arg,
                            valid=True,
                            uid=path))
    elif len(q):
        for match in alp.fuzzy_search(q, projectNames, key=lambda x: x[1]):
            path = match[0]
            title = match[1]
            arg = match[2]
            items.append(I(title=title,
                            subtitle=path,
                            arg=arg,
                            valid=True,
                            uid=path))

    if len(items):
        alp.feedback(items)
    else:
        alp.feedback(I(title="No Matches",
                        subtitle="No recent projects matched your query.",
                        valid=False))


if __name__ == "__main__":
    find_projects()
